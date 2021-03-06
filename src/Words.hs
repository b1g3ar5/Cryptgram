module Words
    (
        decrypt
        , decrypt2
        , decrypt3
        , decrypt4
        , shapeDecrypt
        , scoreCrib
        , addCribs
        , getDict
        , smallDict
        , getShapeDict
        , subDict
        , top
        , nextShapeCribs
        , nextCribs
        , getNext
        , allWordSplits
        , wordLengths
    ) where

import Control.Monad.Logic
import GHC.Exts (sortWith)
import Data.List as L
import Data.Maybe
import Data.Char
import qualified Data.Map as Map
import Data.Tree

import Label
import Crib
import Dict
import Shape
import FBackTrack


wordLengths :: [Int]
wordLengths = [3,2,4,5,6,7,8,9,10,1,11]


allWordSplits :: Dict -> String -> [[String]]
allWordSplits _ [] = [[]]
allWordSplits dd rest = concatMap (\ix ->
                                    fmap (\wds -> (take (wordLengths!!ix) rest) : wds
                                         ) $ allWordSplits dd (drop (wordLengths!!ix) rest)
                                  ) $ filter (\i-> (wordLengths!!i) <= n) [0..10]
  where
    n = length rest

-- | This decrypter uses the ShapeDict dictionary rather than the alphabetical one
-- It is a lot quicker!
shapeDecrypt :: ShapeDict -> [String] -> [String]
shapeDecrypt dict wds = map (applyCrib (head proposedCribs)) wds
    where
        ctIns = reverse $ map fst $ sortWith snd $ zip wds $ map length wds
        -- Work out a list of cribs for each word
        (proposedCribs, _) = foldl (getNext dict) ([emptyCrib], [emptyCrib]) ctIns

getNext :: ShapeDict -> ([Crib], [Crib]) -> String -> ([Crib], [Crib])
getNext dict (proposed, accepted) ct = if null next then (accepted, accepted) else (next, proposed)
    where
        next = nextShapeCribs dict proposed ct

-- | Uses the dictionary and the input cribs to work out potential plain texts tor the input cipher text
-- and expands or contracts the set of cribs over all these.
-- The resulting set of cribs will respect one of the input cribs and a potential plain text.
nextShapeCribs :: ShapeDict -> [Crib] -> String -> [Crib]
nextShapeCribs dict cbs ctIn = concat $ zipWith3 (\pts ct cb -> map (addCrib cb ct) pts) ptss cts cbs
    where
        -- Apply the input cribs to the cipher text
        cts = map (`applyCrib` ctIn) cbs
        -- Get the potential plain texts for each of these texts
        ptss = zipWith (\ct cb -> top 50 (fromJust $ Map.lookup (toShape ct) dict) cb ct) cts cbs


-- | These decryptors use the Dict and Cribs 2, 3 and 4 use backtracking using
-- respectively List, Logic and Stream types
-- Plain decrypt() does a fold over the cipher text working out a new set of cribs each step.

-- | Decrypts a cryptogram
decrypt2 :: Dict -> [String] -> [[String]]
decrypt2 dd wds = do
        wd <- wds
        -- Work out a set of cribs for each word
        cb <- foldl (\cb ct -> nextCribs dd cb ct) [emptyCrib] wds
        -- guard against cribs that don't work for other words
        -- guard $ isWord dd (applyCrib cb wd)
        if isWord dd (applyCrib cb wd) then return () else mzero
        return $ map (applyCrib cb) wds

-- | Decrypts a cryptogram
decrypt3 :: Dict -> [String] -> Logic [String]
decrypt3 dd wds = do
        wd <- (msum .map return) wds
        -- Work out a set of cribs for each word
        cb <- (msum .map return) $ foldl (\cb ct -> nextCribs dd cb ct) [emptyCrib] wds
        -- guard against cribs that don't work for other words
        -- guard $ isWord dd (applyCrib cb wd)
        if isWord dd (applyCrib cb wd) then return () else mzero
        return $ map (applyCrib cb) wds

-- | Decrypts a cryptogram
decrypt4 :: Dict -> [String] -> Stream [String]
decrypt4 dd wds = do
        wd <- (msum .map return) wds
        -- Work out a set of cribs for each word
        cb <- (msum .map return) $ foldl (\cb ct -> nextCribs dd cb ct) [emptyCrib] wds
        -- guard against cribs that don't work for other words
        -- guard $ isWord dd (applyCrib cb wd)
        if isWord dd (applyCrib cb wd) then return () else mzero
        return $ map (applyCrib cb) wds


-- | Decrypts a cryptogram
decrypt :: Dict -> [String] -> [String]
decrypt dd wds = map (\w -> applyCrib (cbs!!0) w) wds
    where
        -- Filter out words not in the dictionary
        filteredWords = filter (\w -> wordCount (subDict dd emptyCrib w) > 0) wds
        ctIns = reverse $ map fst $ sortWith snd $ zip filteredWords $ map length filteredWords
        -- Work out a list of cribs for each word
        cbs = foldl (\cb ct -> nextCribs dd cb ct) [emptyCrib] ctIns

-- | Uses the dictionary and the input cribs to work out potential plain texts tor the input cipher text
-- and expands or contracts the set of cribs over all these.
-- The resulting set of cribs will respect one of the input cribs and a potential plain text.
nextCribs :: Dict -> [Crib] -> String -> [Crib]
nextCribs dd cbs ctIn = concat $ zipWith3 (\pts ct cb -> map (\pt -> addCrib cb ct pt) pts) ptss cts cbs
    where
        -- Apply the input cribs to the cipher text
        cts = map (\cb -> applyCrib cb ctIn) cbs
        -- Get the potential plain texts for each of these texts
        ptss = zipWith (\ct cb -> top 50 dd cb ct) cts cbs

scoreCrib :: Dict -> [String] -> Crib -> Double
scoreCrib dd wds cb = L.sum $ L.map ((wordLogFreq dd).(applyCrib cb)) wds

-- Get the top n fits for a string in a dict with a crib
top :: Int -> Dict -> Crib -> String -> [String]
top n dd cb ct = L.map snd $ take n $ sortWith fst $ L.concatMap sortedToWords $ subDict dd cb ct

-- This function works out all the potential words from a cipher text and a list of cribs
-- and extends the list of cribs to include them all
addCribs :: Dict -> [Crib] -> String -> [Crib]
addCribs dd cbs ct = L.concat $ L.zipWith (\pts cb -> L.map (\pt -> addCrib cb ct pt) pts) ptss cbs
    where
        ptss :: [[String]]
        ptss = L.map (\cb -> top 30 dd cb ct) cbs

-- The compare is a bit crap because the tree is ordered alphabetically - but for cipher text
-- characters this doesn't work
subDict:: Dict -> Crib -> String -> Dict
subDict [] _ _ = []
subDict _ _ [] = []
subDict (t:ts) cb chs@(c:[]) = case compare (lch $ lt) pc of
                                     EQ -> (if (labelIsWord lt) then [Node (Label (lch $ lt) (llogFreq lt) $ 1) []] else []) ++ subDict ts cb chs
                                     LT -> subDict ts cb chs
                                     -- For plain text characters this means no words
                                     -- but for cipher text we need to check the LT trees
                                     GT -> if (isCipherText c) then (subDict ts cb chs) else []
                                where
                                    lt = rootLabel t
                                    -- If it's cipher text and the label character is a potiential fit
                                    -- then make the compare ==
                                    pc = if (isCipherText c)&&((lch lt) `L.elem` (Crib.lookup cb c)) then lch lt else c
subDict (t:ts) cb chs@(c:cs) = case compare (lch $ lt) pc of
                                     EQ -> case sd of
                                                []        -> subDict ts cb chs
                                                _ -> [Node (Label (lch $ lt) (llogFreq lt) $ wordCount sd) $ sd] ++ subDict ts cb chs
                                           where
                                                sd = subDict (subForest t) (Crib.insert cb c (lch lt)) cs
                                     LT -> subDict ts cb chs
                                     GT -> if (isCipherText c) then (subDict ts cb chs) else []
                                where
                                    lt = rootLabel t
                                    -- If it's cipher text and the label character is a potiential fit
                                    -- then make the compare ==
                                    pc = if (isCipherText c)&&((lch lt) `L.elem` (Crib.lookup cb c)) then lch lt else c

-- Note the label saves -log(freq). I did this so they would add over words, but this is
-- not used yet.
getDict::IO Dict
getDict = do
    ls<-readFile("./data/Gutenberg2006.txt")
    let wds = L.map toWordAndFreq $ take 50000 $ lines ls
    let s = L.foldl (\d wf -> Dict.addWord d (fst wf) (snd wf)) ([]::Dict) wds
    return s
    where
        toWordAndFreq ss = (w, f)
            where
                -- The line is [rank, word, freq]
                ws = words ss
                w = L.filter (\c -> c `L.elem` alphabet) $ L.map toLower $ ws!!1
                f =  -(log $ (read $ ws!!2)*0.000000001) -- the number in the file is occurrences in 1bn words

-- Note the label saves -log(freq). I did this so they would add over words, but this is
-- not used yet.
getShapeDict::IO ShapeDict
getShapeDict = do
    ls<-readFile("./data/Gutenberg2006.txt")
    let wds = L.map toWordAndFreq $ lines ls
    let s = L.foldl (\d wf -> Shape.addWord d (fst wf) (snd wf)) (Map.empty) wds
    return s
    where
        toWordAndFreq ss = (w, f)
            where
                -- The line is [rank, word, freq]
                ws = words ss
                w = L.filter (\c -> c `L.elem` alphabet) $ L.map toLower $ ws!!1
                f =  -(log $ (read $ ws!!2)*0.000000001) -- the number in the file is occurrences in 1bn words


smallDict::IO Dict
smallDict = do
    ls<-readFile("./data/SmallDict.txt")
    let wds = L.map toWordAndFreq $ lines ls
    let s = L.foldl (\d wf -> Dict.addWord d (fst wf) (snd wf)) ([]::Dict) wds
    return s
    where
        toWordAndFreq ss = (w, f)
            where
                -- The line is [rank, word, freq]
                ws = words ss
                w = L.filter (\c -> c `L.elem` alphabet) $ L.map toLower $ ws!!1
                f =  -(log $ (read $ ws!!2)*0.000000001) -- the number in the file is occurrences in 1bn words
