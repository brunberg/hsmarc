{-# LANGUAGE OverloadedStrings #-} 

-- Converted to Haskell function
-- Eric Petersen
-- 17 July 2010
-- TODO: add special processing of amp and semi-colon


-- original file name TEXT21.txt
-- found at http://www.loc.gov/marc/makrbrkr.html                                                                         
--   MARC21-TO-TEXT                         DATE: July 3, 2002              
--   CHARACTER CONVERSION TABLE FOR MARCBrkr Version 2.5                    
--                                                                          
--   Source: Randall K. Barry (RBAR@LOC.GOV)
--           Library of Congress
--           Network Development and MARC Standards Office  
--           Washington, D.C. 20540-4102  U.S.A.


--  Convert any hexadecimal (or decimal) value listed to its mnemonic.
--    Other hexadecimal (or decimal) values should be passed to the text
--    file unchanged.

--  NOTE: Hexadecimal values 1D, 1E, and 1F are not listed since they have 
--    a special function in MARC records. 

--  Special processing should be applied to character strings in the MARC 
--    record beginning with an ampersand "&" (26x/038d), if the ampersand 
--    is not followed by a space.  The string, including the introductory 
--    ampersand, should be read through the first semicolon ";" (3Bx/059d).
--    If the semicolon is not encountered within 9 characters, including
--    the ampersand, do not apply this special processing.  The ampersand 
--    and semicolon should be replaced by the opening and closing curly
--    braces "{" (7Bx/123d) and "}" (7Dx/125d), respectively, to generate 
--    the corresponding mnemonic for the text file.  The curly braces are 
--    used in MARCMakr Version 2.5 to delimit mnemonics. 
--    Thus "&zilch;" in the input MARC record file would be come "{zilch}" 
--    in the output text file. 

--  Mnemonics in this table are always enclosed in curly braces "{...}". 

--  Columns in this table are delimited by a comma ","

--  hex value -> mnemonic --name/comment
--  ??,        {...}       [character name (FORMAT)]

module Data.Marc21.MnemText21 (

    hexToMnem

    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
 
-- Interface, in case implementation is changed
hexToMnem :: ByteString -> ByteString
hexToMnem = C.concatMap hToM

-- Helper function
-- TODO: Might make into an assoc. array using Data.Map
hToM :: Char -> ByteString
hToM chr = case chr of
    '\x00' -> "{00}" -- hex value 00 (MARC 21)
    '\x01' -> "{01}" -- hex value 01 (MARC 21)
    '\x02' -> "{02}" -- hex value 02 (MARC 21)
    '\x03' -> "{03}" -- hex value 03 (MARC 21)
    '\x04' -> "{04}" -- hex value 04 (MARC 21)
    '\x05' -> "{05}" -- hex value 05 (MARC 21)
    '\x06' -> "{06}" -- hex value 06 (MARC 21)
    '\x07' -> "{07}" -- hex value 07 (MARC 21)
    '\x08' -> "{08}" -- hex value 08 (MARC 21)
    '\x09' -> "{09}" -- hex value 09 (MARC 21)
    '\x0A' -> "{0A}" -- hex value 0A (MARC 21)
    '\x0B' -> "{0B}" -- hex value 0B (MARC 21)
    '\x0C' -> "{0C}" -- hex value 0C (MARC 21)
    '\x0D' -> "{0D}" -- hex value 0D (MARC 21)
    '\x0E' -> "{0E}" -- hex value OE (MARC 21)
    '\x0F' -> "{0F}" -- hex value OF (MARC 21)
    '\x10' -> "{10}" -- hex value 10 (MARC 21)
    '\x11' -> "{11}" -- hex value 11 (MARC 21)
    '\x12' -> "{12}" -- hex value 12 (MARC 21)
    '\x13' -> "{13}" -- hex value 13 (MARC 21)
    '\x14' -> "{14}" -- hex value 14 (MARC 21)
    '\x15' -> "{15}" -- hex value 15 (MARC 21)
    '\x16' -> "{16}" -- hex value 16 (MARC 21)
    '\x17' -> "{17}" -- hex value 17 (MARC 21)
    '\x18' -> "{18}" -- hex value 18 (MARC 21)
    '\x19' -> "{19}" -- hex value 19 (MARC 21)
    '\x1A' -> "{1A}" -- hex value 1A (MARC 21)
    '\x1B' -> "{esc}" -- escape (MARC 21)
    '\x1C' -> "{1C}" -- hex value 1C (MARC 21)
    '\x24' -> "{dollar}" -- dollar sign (MARC 21)
    '\x7B' -> "{lcub}" -- opening curly brace (MARC 21)
    '\x7D' -> "{rcub}" -- closing curly brace (MARC 21)
    '\x7F' -> "{7F}" -- hex value 7F (MARC 21)
    '\x80' -> "{80}" -- hex value 80 (MARC 21)
    '\x81' -> "{81}" -- hex value 81 (MARC 21)
    '\x82' -> "{82}" -- hex value 82 (MARC 21)
    '\x83' -> "{83}" -- hex value 83 (MARC 21)
    '\x84' -> "{84}" -- hex value 84 (MARC 21)
    '\x85' -> "{85}" -- hex value 85 (MARC 21)
    '\x86' -> "{86}" -- hex value 86 (MARC 21)
    '\x87' -> "{87}" -- hex value 87 (MARC 21)
    '\x88' -> "{88}" -- hex value 88 (MARC 21)
    '\x89' -> "{89}" -- hex value 89 (MARC 21)
    '\x8A' -> "{8A}" -- hex value 8A (MARC 21)
    '\x8B' -> "{8B}" -- hex value 8B (MARC 21)
    '\x8C' -> "{8C}" -- hex value 8C (MARC 21)
    '\x8D' -> "{joiner}" -- zero width joiner (MARC 21)
    '\x8E' -> "{nonjoin}" -- zero width non-joiner (MARC 21)
    '\x8F' -> "{8F}" -- hex value 8F (MARC 21)
    '\x90' -> "{90}" -- hex value 90 (MARC 21)
    '\x91' -> "{91}" -- hex value 91 (MARC 21)
    '\x92' -> "{92}" -- hex value 92 (MARC 21)
    '\x93' -> "{93}" -- hex value 93 (MARC 21)
    '\x94' -> "{94}" -- hex value 94 (MARC 21)
    '\x95' -> "{95}" -- hex value 95 (MARC 21)
    '\x96' -> "{96}" -- hex value 96 (MARC 21)
    '\x97' -> "{97}" -- hex value 97 (MARC 21)
    '\x98' -> "{98}" -- hex value 98 (MARC 21)
    '\x99' -> "{99}" -- hex value 99 (MARC 21)
    '\x9A' -> "{9A}" -- hex value 9A (MARC 21)
    '\x9B' -> "{9B}" -- hex value 9B (MARC 21)
    '\x9C' -> "{9C}" -- hex value 9C (MARC 21)
    '\x9D' -> "{9D}" -- hex value 9D (MARC 21)
    '\x9E' -> "{9E}" -- hex value 9E (MARC 21)
    '\x9F' -> "{9F}" -- hex value 9F (MARC 21)
    '\xA0' -> "{A0}" -- hex value A0 (MARC 21)
    '\xA1' -> "{Lstrok}" -- latin capital letter l with stroke (MARC 21)
    '\xA2' -> "{Ostrok}" -- latin capital letter o with stroke (MARC 21)
    '\xA3' -> "{Dstrok}" -- latin capital letter d with stroke (MARC 21)
    '\xA4' -> "{THORN}" -- latin capital letter thorn (icelandic) (MARC 21)
    '\xA5' -> "{AElig}" -- latin capital letter AE (MARC 21)
    '\xA6' -> "{OElig}" -- latin capital letter OE (MARC 21)
    '\xA7' -> "{softsign}" -- modifier letter soft sign (MARC 21)
    '\xA8' -> "{middot}" -- middle dot (MARC 21)
    '\xA9' -> "{flat}" -- musical flat sign (MARC 21)
    '\xAA' -> "{reg}" -- registered sign (MARC 21)
    '\xAB' -> "{plusmn}" -- plus-minus sign (MARC 21)
    '\xAC' -> "{Ohorn}" -- latin capital letter o with horn (MARC 21)
    '\xAD' -> "{Uhorn}" -- latin capital letter u with horn (MARC 21)
    '\xAE' -> "{mlrhring}" -- modifier letter right half ring (alif) (MARC 21)
    '\xAF' -> "{AF}" -- hex value AF (MARC 21)
    '\xB0' -> "{mllhring}" -- modifier letter left half ring (ayn) (MARC 21)
    '\xB1' -> "{lstrok}" -- latin small letter l with stroke (MARC 21)
    '\xB2' -> "{ostrok}" -- latin small letter o with stroke (MARC 21)
    '\xB3' -> "{dstrok}" -- latin small letter d with stroke (MARC 21)
    '\xB4' -> "{thorn}" -- latin small letter thorn (icelandic) (MARC 21)
    '\xB5' -> "{aelig}" -- latin small letter ae (MARC 21)
    '\xB6' -> "{oelig}" -- latin small letter oe (MARC 21)
    '\xB7' -> "{hardsign}" -- modifier letter hard sign (MARC 21)
    '\xB8' -> "{inodot}" -- latin small letter dotless i (MARC 21)
    '\xB9' -> "{pound}" -- pound sign (MARC 21)
    '\xBA' -> "{eth}" -- latin small letter eth (MARC 21)
    '\xBB' -> "{BB}" -- hex value BB (MARC 21)
    '\xBC' -> "{ohorn}" -- latin small letter o with horn (MARC 21)
    '\xBD' -> "{uhorn}" -- latin small letter u with horn (MARC 21)
    '\xBE' -> "{BE}" -- hex value BE (MARC 21)
    '\xBF' -> "{BF}" -- hex value BF (MARC 21)
    '\xC0' -> "{deg}" -- degree sign (MARC 21)
    '\xC1' -> "{scriptl}" -- latin small letter script l (MARC 21)
    '\xC2' -> "{phono}" -- sound recording copyright (MARC 21)
    '\xC3' -> "{copy}" -- copyright sign (MARC 21)
    '\xC4' -> "{sharp}" -- sharp (MARC 21)
    '\xC5' -> "{iquest}" -- inverted question mark (MARC 21)
    '\xC6' -> "{iexcl}" -- inverted exclamation mark (MARC 21)
    '\xC7' -> "{C7}" -- hex value C7 (MARC 21)
    '\xC8' -> "{C8}" -- hex value C8 (MARC 21)
    '\xC9' -> "{C9}" -- hex value C9 (MARC 21)
    '\xCA' -> "{CA}" -- hex value CA (MARC 21)
    '\xCB' -> "{CB}" -- hex value CB (MARC 21)
    '\xCC' -> "{CC}" -- hex value CC (MARC 21)
    '\xCD' -> "{CD}" -- hex value CD (MARC 21)
    '\xCE' -> "{CE}" -- hex value CE (MARC 21)
    '\xCF' -> "{CF}" -- hex value CF (MARC 21)
    '\xD0' -> "{D0}" -- hex value D0 (MARC 21)
    '\xD1' -> "{D1}" -- hex value D1 (MARC 21)
    '\xD2' -> "{D2}" -- hex value D2 (MARC 21)
    '\xD3' -> "{D3}" -- hex value D3 (MARC 21)
    '\xD4' -> "{D4}" -- hex value D4 (MARC 21)
    '\xD5' -> "{D5}" -- hex value D5 (MARC 21)
    '\xD6' -> "{D6}" -- hex value D6 (MARC 21)
    '\xD7' -> "{D7}" -- hex value D7 (MARC 21)
    '\xD8' -> "{D8}" -- hex value D8 (MARC 21)
    '\xD9' -> "{D9}" -- hex value D9 (MARC 21)
    '\xDA' -> "{DA}" -- hex value DA (MARC 21)
    '\xDB' -> "{DB}" -- hex value DB (MARC 21)
    '\xDC' -> "{DC}" -- hex value DC (MARC 21)
    '\xDD' -> "{DD}" -- hex value DD (MARC 21)
    '\xDE' -> "{DE}" -- hex value DE (MARC 21)
    '\xDF' -> "{DF}" -- hex value DF (MARC 21)
    '\xE0' -> "{hooka}" -- combining hook above (MARC 21)
    '\xE1' -> "{grave}" -- combining grave (MARC 21)
    '\xE2' -> "{acute}" -- combining acute (MARC 21)
    '\xE3' -> "{circ}" -- combining circumflex (MARC 21)
    '\xE4' -> "{tilde}" -- combining tilde (MARC 21)
    '\xE5' -> "{macr}" -- combining macron (MARC 21)
    '\xE6' -> "{breve}" -- combining breve (MARC 21)
    '\xE7' -> "{dot}" -- combining dot above (MARC 21)
--    '\xE8' -> "{diaer}" -- combining diaeresis (MARC 21)  
    '\xE8' -> "{uml}" -- combining umlaut (MARC 21)
    '\xE9' -> "{caron}" -- combining hacek (MARC 21)
    '\xEA' -> "{ring}" -- combining ring above (MARC 21)
    '\xEB' -> "{llig}" -- combining ligature left half (MARC 21)
    '\xEC' -> "{rlig}" -- combining ligature right half (MARC 21)
    '\xED' -> "{rcommaa}" -- combining comma above right (MARC 21)
    '\xEE' -> "{dblac}" -- combining double acute (MARC 21)
    '\xEF' -> "{candra}" -- combining candrabindu (MARC 21)
    '\xF0' -> "{cedil}" -- combining cedilla (MARC 21)
    '\xF1' -> "{ogon}" -- combining ogonek (MARC 21)
    '\xF2' -> "{dotb}" -- combining dot below (MARC 21)
    '\xF3' -> "{dbldotb}" -- combining double dot below (MARC 21)
    '\xF4' -> "{ringb}" -- combining ring below (MARC 21)
    '\xF5' -> "{dblunder}" -- combining double underscore (MARC 21)
    '\xF6' -> "{under}" -- combining underscore (MARC 21)
    '\xF7' -> "{commab}" -- combining comma below (MARC 21)
    '\xF8' -> "{rcedil}" -- combining right cedilla (MARC 21)
    '\xF9' -> "{breveb}" -- combining breve below (MARC 21)
    '\xFA' -> "{ldbltil}" -- combining double tilde left half (MARC 21)
    '\xFB' -> "{rdbltil}" -- combining double tilde right half (MARC 21)
    '\xFC' -> "{FC}" -- hex value FC (MARC 21)
    '\xFD' -> "{FD}" -- hex value FD (MARC 21)
    '\xFE' -> "{commaa}" -- combining comma above (MARC 21)
    '\xFF' -> "{FF}" -- hex value FF (MARC 21)
    _      -> C.singleton chr -- all other characters
    