{-# LANGUAGE OverloadedStrings #-} 
-- File of constants and convenience functions for use with MaRC21 

module Data.Marc21.Constants where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C

-- Constants
ft :: Char
ft  = '\x1E'   -- Field Terminator (ASCII '\RS' : Record separator)
rt :: Char
rt  = '\x1D'   -- Record Terminator (ASCII '\GS' : Group separator)
dlm :: Char
dlm = '\x1F'   -- Delimiter (ASCII '\US' : Unit separator)

-- Pre-packed constants
ftP :: ByteString
ftP    = C.singleton '\x1E'   -- Field Terminator (ASCII '\RS' : Record sep.)
rtP :: ByteString
rtP    = C.singleton '\x1D'   -- Record Terminator (ASCII '\GS' : Group sep.)
dlmP :: ByteString
dlmP   = C.singleton '\x1F'   -- Delimiter (ASCII '\US' : Unit sep.)
spaceP :: ByteString
spaceP = C.singleton ' '      -- Space
bsP :: ByteString
bsP    = C.singleton '\\'     -- Backslash

-- internal convenience functions
(<%>) :: ByteString -> ByteString-> ByteString
p <%> q = p `C.append` q

-- | /O(n)/ Splits a 'Text' into components of length @k@.  The last
-- element may be shorter than the other chunks, depending on the
-- length of the input. Examples:
--
-- > chunksOf 3 "foobarbaz"   == ["foo","bar","baz"]
-- > chunksOf 4 "haskell.org" == ["hask","ell.","org"]
chunksOf :: Int -> ByteString -> [ByteString]
chunksOf k = go
  where
    go t = case C.splitAt k t of
             (a,b) | C.null a    -> []
                   | otherwise -> a : go b
{-# INLINE chunksOf #-}

-- > justifyRight 7 'x' "bar"    == "xxxxbar"
-- > justifyRight 3 'x' "foobar" == "foobar"
justifyRight :: Int -> Char -> ByteString -> ByteString
justifyRight k c t
    | len >= k  = t
    | otherwise = filler <%> t
  where len = C.length t
        filler = C.replicate (k-len) c
{-# INLINE justifyRight #-}


replaceSpaceWith  :: ByteString    -- ^ Replacement text            
                   -> ByteString    -- ^ Input text    
                   -> ByteString                 
replaceSpaceWith d = C.intercalate d . C.split ' '
{-# INLINE replaceSpaceWith #-}

