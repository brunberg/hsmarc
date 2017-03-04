{-# LANGUAGE OverloadedStrings #-} 

-- TODO: make an output similar to PerlMarc?

--LDR 00397nam  22001458a 4500
--245 10 _aProgramming Perl / 
--       _cLarry Wall, Tom Christiansen & Jon Orwant.
--650  0 _aPerl (Computer program language)
--700 1  _aChristiansen, Tom.
--700 1  _aOrwant, Jon.


module Data.Marc21.Mnem 
( recordsToMnem ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import           Data.Marc21
import           Data.Marc21.Constants
import           Data.Marc21.MnemText21

recordsToMnem :: [Record] -> [ByteString]
recordsToMnem = map marc8toMnem

marc8toMnem :: Record -> ByteString
marc8toMnem x = renderLDR x <%> renderFields x

renderLDR :: Record -> ByteString
renderLDR x = C.pack "=LDR  " <%> ldr x <%> C.singleton '\n'

renderFields :: Record -> ByteString
renderFields rcd = C.unlines $ map renderField (fields rcd)

renderField :: Field -> ByteString
renderField (VarField t i1 i2 f) = renderVarField t i1 i2 <%> renderSubFlds f
renderField (CntlField t d)      = renderCntrlField t <%> cntlData
   where cntlData = replaceSpaceWith bsP d

renderCntrlField :: ByteString -> ByteString
renderCntrlField t     = '=' `C.cons` t <%> C.pack "  "

renderVarField :: ByteString -> Char -> Char -> ByteString
renderVarField t i1 i2 = '=' `C.cons` t <%> C.pack "  " <%> indr1 <%> indr2
    where indMod c = if c == ' ' then bsP else C.singleton c
          indr1 = indMod i1
          indr2 = indMod i2

renderSubFlds :: [(ByteString, ByteString)] -> ByteString          
renderSubFlds f = C.concat $ map fld f
    where fld (s,d)   = subFldInd s <%> subfData d
          subFldInd s = '$' `C.cons` s
          subfData = hexToMnem
