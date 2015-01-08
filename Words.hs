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
        , getShapeDict
        , subDict
        , top
        , nextShapeCribs
        , nextCribs
        , getNext
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


-- There's a problem here when one of the initial words is not in the dictionary, but has a 
-- word of the same shape in the dictionary. This narrows down the crib set too much and we
-- need to find a way of throwing the cipher text and it's cribs away and starting on the
-- next word.

-- | This decrypter uses the ShapeDict dictionary rather than the alphabetical one
-- It is a lot quicker!
shapeDecrypt :: ShapeDict -> [String] -> [String]
shapeDecrypt dd wds = map (\w -> applyCrib (proposedCribs!!0) w) wds
    where
        ctIns = reverse $ map fst $ sortWith snd $ zip wds $ map length wds            
        -- Work out a list of cribs for each word
        (proposedCribs, acceptedCribs) = foldl (getNext dd) ([emptyCrib], [emptyCrib]) ctIns

getNext :: ShapeDict -> ([Crib], [Crib]) -> String -> ([Crib], [Crib])
getNext dd (proposed, accepted) ct = if (length next == 0) then (accepted, accepted) else (next, proposed)
    where
        next = nextShapeCribs dd proposed ct

-- | Uses the dictionary and the input cribs to work out potential plain texts tor the input cipher text
-- and expands or contracts the set of cribs over all these.
-- The resulting set of cribs will respect one of the input cribs and a potential plain text.
nextShapeCribs :: ShapeDict -> [Crib] -> String -> [Crib]
nextShapeCribs dd cbs ctIn = concat $ zipWith3 (\pts ct cb -> map (\pt -> addCrib cb ct pt) pts) ptss cts cbs
    where
        -- Apply the input cribs to the cipher text
        cts = map (\cb -> applyCrib cb ctIn) cbs                
        -- Get the potential plain texts for each of these texts
        ptss = zipWith (\ct cb -> top 50 (fromJust $ Map.lookup (toShape ct) dd) cb ct) cts cbs    


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
subDict (t:ts) cb chs@(c:[]) = case compare (ch $ lt) pc of
                                     EQ -> (if (labelIsWord lt) then [Node (Label (ch $ lt) (logFreq lt) $ 1) []] else []) ++ subDict ts cb chs
                                     LT -> subDict ts cb chs
                                     -- For plain text characters this means no words
                                     -- but for cipher text we need to check the LT trees
                                     GT -> if (isCipherText c) then (subDict ts cb chs) else []
                                where
                                    lt = rootLabel t
                                    -- If it's cipher text and the label character is a potiential fit
                                    -- then make the compare ==
                                    pc = if (isCipherText c)&&((ch lt) `L.elem` (Crib.lookup cb c)) then ch lt else c
subDict (t:ts) cb chs@(c:cs) = case compare (ch $ lt) pc of
                                     EQ -> case sd of 
                                                []        -> subDict ts cb chs
                                                otherwise -> [Node (Label (ch $ lt) (logFreq lt) $ wordCount sd) $ sd] ++ subDict ts cb chs
                                           where
                                                sd = subDict (subForest t) (Crib.insert cb c (ch lt)) cs
                                     LT -> subDict ts cb chs
                                     GT -> if (isCipherText c) then (subDict ts cb chs) else []
                                where
                                    lt = rootLabel t
                                    -- If it's cipher text and the label character is a potiential fit
                                    -- then make the compare ==
                                    pc = if (isCipherText c)&&((ch lt) `L.elem` (Crib.lookup cb c)) then ch lt else c

-- Note the label saves -log(freq). I did this so they would add over words, but this is
-- not used yet.
getDict::IO Dict
getDict = do
    ls<-readFile("./data/Gutenberg2006.txt")
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




