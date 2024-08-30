{-# LANGUAGE BangPatterns, OverloadedStrings #-} 

-----------------------------------------------------------------------------
-- |
-- Module      :  Marc21
-- Copyright   :  (c) Eric J. Petersen 2010
-- License     :  BSD3
--
-- Maintainer  :  ejptrsn@gmail.com 
-- Stability   :  experimental
-- Portability :  
--
-- MARC 21 Format for Bibliographic Data
-- http://www.loc.gov/marc/
--
-----------------------------------------------------------------------------

module Data.Marc21 (

    -- * Data structures
      Record (..)
    , Field (..)
    
    -- * Working with files
    , marcReader     -- :: ByteString -> [Record]
    , marcWriter     -- :: [Record] -> ByteString
    
    -- * Accessor functions
    --  NOTE: API may change
    , getFieldFmtd
    , getFieldValue
    , getFieldValueList
    , getSubFieldValue
    , getSubFieldValueList
    , addVarField
        
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import           Data.Marc21.Constants

-- Data Structures
data Record = Record 
        { ldr        :: !ByteString
        , fields     :: ![Field]
        } deriving (Show)
        
data Field = CntlField 
                { tag      :: !ByteString
                , tagData  :: !ByteString
                } 
             | VarField 
                { tag     :: !ByteString
                , ind1    :: !Char
                , ind2    :: !Char
                , subfs   :: ![(ByteString, ByteString)]
                } deriving (Show)
                
-- Functions for working with batches of records
marcReader :: ByteString -> [Record]
marcReader file = map putRecord (init $ C.split rt file)

marcWriter :: [Record] -> ByteString
marcWriter rcds = C.concat $ map encodeRecord rcds

-- Functions to extract info from  marc records
-- NOTE: API may change

-- Helper function to format a field

formatWithSpace :: [(t, ByteString)] -> ByteString
formatWithSpace sfs = C.intercalate spaceP $ map snd sfs

-- isTag :: String -> Field -> Bool
-- isTag targetTag fld = C.pack targetTag == tag fld

-- filterByTag :: String -> Record -> [Field]
-- filterByTag tgtTag rcd = filter (isTag tgtTag) $ fields rcd

filterByTags :: [String] -> Record -> [Field]
filterByTags fldTags rcd = [fld | fld <- fields rcd, tag fld `elem` tagList] 
    where tagList = map C.pack fldTags

getFieldFmtd :: [String] -> Record -> ByteString
getFieldFmtd tgtTags rcd = 
    C.concat $ map (fmt . subfs) (filterByTags tgtTags rcd)
    where fmt sfs       = C.concat $ map delimit sfs
          delimit (s,d) = '$' `C.cons` s <%> d 
          --C.concat [fmt (subfs fld) | fld <- fields rcd, tag fld == C.pack fldTag]
    
getFieldValue :: [String] -> Record -> ByteString
getFieldValue fldTags rcd = C.unlines $ getFieldValueList fldTags rcd
    
getFieldValueList :: [String] -> Record -> [ByteString]
getFieldValueList fldTags rcd = 
    map (formatWithSpace . subfs) (filterByTags fldTags rcd)
    
    
getSubFieldValue :: ([String], String) -> Record -> ByteString
getSubFieldValue (fldTag, sfis) rcd = C.unlines $ getSubFieldValueList (fldTag, sfis) rcd
    
getSubFieldValueList :: ([String], String) -> Record -> [ByteString]
getSubFieldValueList (fldTags, sfis) rcd =
    map (fmtSubFieldsWithSpace sfis) (filterByTags fldTags rcd)

fmtSubFieldsWithSpace :: String -> Field -> ByteString
fmtSubFieldsWithSpace sfs fld = C.intercalate spaceP filterBySubFields
    where filterBySubFields = [d | (s,d) <- subfs fld, s `elem` listSfs] 
          listSfs = map C.singleton sfs
          
addVarField
  :: String
     -> Char
     -> Char
     -> [(ByteString, ByteString)]
     -> Record
     -> Record
addVarField t i1 i2 sfs r = r {fields = newFields}  
    where newFields = fields r ++ [VarField (C.pack t) i1 i2 sfs]
    
--             
-- Functions for parsing
--
extract :: (Int, Int) -> ByteString -> ByteString
extract (!len,!offset) s 
        = C.take (fromIntegral len) $ C.drop (fromIntegral offset) s
        
putRecord :: ByteString -> Record
putRecord s = Record nleader nfields
    where nleader = extract (24,0) s
          nfields = extractFields dir chunk
              where dir     = dirOfFlds (C.takeWhile (/= ft) (C.drop 24 s))
                    chunk   = extract (len, baseAdd) s
                    len     = reclen - baseAdd
                    reclen  = read (C.unpack $ C.take 5 nleader) :: Int
                    baseAdd = read (C.unpack $ extract (5,12) nleader) :: Int
                      
extractFields :: [(ByteString, Int, Int)] -> ByteString -> [Field]
extractFields dir info = [ eachField (t,l,o) info | (t,l,o) <- dir ]

eachField :: (ByteString, Int, Int) -> ByteString -> Field
eachField (marcTag,len,offset) info  | tagNum < 10  = CntlField marcTag tagDataR
                                 | otherwise    = VarField marcTag i1 i2 subf
    where tagNum    = read (C.unpack marcTag) :: Int
          tagDataR  = extract (len-1,offset) info
          i1        = C.head tagDataR
          i2        = C.head $ C.drop 1 tagDataR
          subf      = map (C.splitAt 1) subf1    -- ("a", "<subFldData>")
          subf1     = tail $ C.splitWith (== dlm) $ C.drop 2 tagDataR
       
dirOfFlds :: ByteString -> [(ByteString, Int, Int)]
dirOfFlds xs = [ taginfo x | x <- chunksOf 12 xs ]
    where taginfo x = (marcTag, fldLength, offset)
              where marcTag   = C.take 3 x
                    fldLength = read (C.unpack $ extract (4,3) x) :: Int
                    offset    = read (C.unpack $ C.drop 7 x) :: Int
                     
encodeRecord :: Record -> ByteString       
encodeRecord x = newRec
    where (dir, accFld)  = makeDirOfTags (C.empty, 0, C.empty) $ fields x
          newBaseAdd     = reverse $ take 5 $ reverse (show $ C.length dir + 25) ++ repeat '0'
          tmpLdr         = C.take 12 (ldr x) <%> C.pack newBaseAdd <%> C.drop 17 (ldr x)
          newRec1        = C.drop 5 tmpLdr <%> dir <%> ftP <%> accFld <%> rtP
          newRecSize1    = 5 + C.length newRec1
          newRecSize     = justifyRight 5 '0' (C.pack (show newRecSize1))
          newRec         = newRecSize <%> newRec1 
  
-- 'makeDirOfTags' constructs the directory section of the MaRC file 
-- The directory is built by accumulating the directory while traversing fields. 
makeDirOfTags
  :: (ByteString, Int, ByteString) -> [Field] -> (ByteString, ByteString)
makeDirOfTags (dir,_,accFld) []                             = (dir,accFld)
makeDirOfTags (dir,os,accFld) (CntlField t d : fs)           = 
              makeDirOfTags (newDir,newOffset,newAccFld) fs
    where newData                      = d `C.snoc` ft
          (newDir,newOffset,newAccFld) = newDirData t newData dir os accFld
          
makeDirOfTags (dir,os,accFld) (VarField t i1 i2 flds   : fs) = 
              makeDirOfTags (newDir,newOffset,newAccFld) fs
    where newData1   = C.concat [dlm `C.cons` s <%> d  | (s,d) <- flds]
          newData    = i1 `C.cons` ((i2 `C.cons` newData1) `C.snoc` ft)
          (newDir,newOffset,newAccFld) = newDirData t newData dir os accFld

-- 'newDirData' is a helper function to format portions of the directory       
newDirData
  :: ByteString
     -> ByteString
     -> ByteString
     -> Int
     -> ByteString
     -> (ByteString, Int, ByteString)
newDirData t nd dir os accFld  = (newDir,newOffset,newAccFld)
    where dataLen     = C.length nd
          dataLenFmtd = justifyRight 4 '0' (C.pack (show dataLen))
          newOffset   = os + fromIntegral dataLen
          osFmtd      = justifyRight 5 '0' (C.pack (show os))
          newDir      = dir <%> t <%> dataLenFmtd <%> osFmtd
          newAccFld   = accFld <%> nd
    
