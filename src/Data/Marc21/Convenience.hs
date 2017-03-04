{-# LANGUAGE OverloadedStrings #-} 

module Data.Marc21.Convenience (

    -- * Convenience functions
      title
    , titleProper
    , isbns
    , isbnsList
    , edition
    , pubYear
    , publisher
    , mainAuthor
    , uniformTitle
    , subjectHeadings
    , addedEntries
    , notes
    
    )where

import Data.ByteString (ByteString)
import Data.Marc21

--
-- Convenience functions
-- NOTE: API may change

-- TODO:
-- deleteField

title :: Record -> ByteString
title = getSubFieldValue (["245"], "ab")

-- title_proper: 245 $a$n$p
titleProper :: Record -> ByteString
titleProper = getSubFieldValue (["245"], "anp")

-- isbn: 020 $a
-- Returns list of isbns
isbns :: Record -> ByteString
isbns = getSubFieldValue (["020"], "a")

isbnsList :: Record -> [ByteString]
isbnsList = getSubFieldValueList (["020"], "a")

-- edition: 250 $a
edition :: Record -> ByteString
edition = getSubFieldValue (["250"], "a")

-- pubYear: 260 $c
pubYear :: Record -> ByteString
pubYear = getSubFieldValue (["260"], "c")

-- publisher: 260 $b
publisher :: Record -> ByteString
publisher = getSubFieldValue (["260"], "b")

-- author: 100, 110, or 111  
mainAuthor :: Record -> ByteString
mainAuthor = getFieldValue ["100", "110", "111"]

-- uniformTitle: 130 or 240
uniformTitle :: Record -> ByteString
uniformTitle = getFieldValue ["130", "240"]

-- subjects 
subjectHeadings :: Record -> ByteString
subjectHeadings = getFieldValue ["600","610","611","630","648","650","651",
                           "653","654","655","656","657","658","662", 
                           "690","691","696","697","698","699"]

-- addedEntries
addedEntries :: Record -> ByteString
addedEntries = getFieldValue ["700","710","711","720","730","740",
                               "752","753","754","790","791","792",
                               "793","796","797","798","799"]

-- notes
notes :: Record -> ByteString
notes = getFieldValue ["500", "501", "502", "504", "505", "506", "507",
                        "508", "510", "511", "513", "514", "515", "516",
                        "518", "520", "521", "522", "524", "525", "526", 
                        "530", "533", "534", "535", "536", "538", "540", 
                        "541", "544", "545", "546", "547", "550", "552",
                        "555", "556", "561", "562", "563", "565", "567",
                        "580", "581", "583", "584", "585", "586", "590",
                        "591", "592", "593", "594", "595", "596", "597",
                        "598", "599"]
