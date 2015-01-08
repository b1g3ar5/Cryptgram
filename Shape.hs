module Shape
    (
    ShapeDict(..)
    , toShape
    , Shape.addWord
    ) where

import Data.Map as M
import Data.List as L

-- | This file has the types for a doctionary where words are filed by the shape that
-- they have in terms if letter.

import Dict

-- | The shape of a word is an integer where the digits represent the letters.
-- Each new letter in the word is assiged a digit, repeated letters always get the same digit
-- Words with more than 9 different letters just get paddewd with zeros on the end
-- eg. "ODD" = 122, "POOP" = 1221, "POPULATION" = 1213456728, "INFATUATION" = 12345645172
type Shape = Int

-- Works out the shape of a word
toShape :: String -> Shape
toShape [] = 0
toShape (c:[]) = 1
toShape cs = L.foldl (\acc (a,b) -> acc + a*10^b) 0 $ zip (go [] M.empty cs) [0..]
    where
        go :: [Int] -> M.Map Char Int -> String -> [Int]
        go ns _ [] = reverse ns
        go ns mp (c:cs) = go (ns ++ [ix]) newMp cs
            where
                n = size mp
                newMp = insertWith (\new old -> old) c (n+1) mp
                ix = newMp!c

-- The shape dictionary saves words according to their shape. Words of the same shape are stored in a Dict (ie. a Rose Tree)
type ShapeDict = Map Int Dict

-- | Adds a word to a ShapeDict
addWord :: ShapeDict -> String -> Double -> ShapeDict
addWord dd wd f = insertWith (\new old -> Dict.addWord old wd f) sh newdd dd
    where
        sh = toShape wd
        newdd = Dict.addWord [] wd f

