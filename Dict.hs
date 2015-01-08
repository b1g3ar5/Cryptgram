module Dict
    (
        Dict(..)
        , isWord
        , addWord
        , wordCount
        , wordLogFreq
        , sortedToWords
    ) where

import GHC.Exts (sortWith)
import Data.Monoid
import Data.Map as M
import Data.List as L
import Data.Tree

import Label

-- This file creates a dictionary from a text file as a Rose Tree
-- This works well for lookups, I hope.

type Dict = Forest Label

-- Extend the Tree instance
class Functor w => Comonad w where
   (=>>)    :: w a -> (w a -> b) -> w b
   coreturn :: w a -> a
   cojoin     :: w a -> w (w a)
   x =>> f = fmap f (cojoin x)
          
-- Not sure why I did this, it's not used anywhere, yet.
instance Comonad Tree where
    coreturn (Node r ts) = r
    cojoin  t = Node t (L.map cojoin $ subForest t)

instance Monoid Int where
    mempty = 0
    m `mappend` n = m+n

-- Counts the words in a dictionary
wordCount::Dict->Int
wordCount d = L.sum $ L.map (count . rootLabel) d

-- | The probability of one of the words in a dictionary
freqDict :: Dict -> Double
freqDict dd = L.sum $ L.map (exp . ((-1.0)*) . logFreq . rootLabel) dd

-- | Adds a new word (with it's logFreq) to a dictionary
addWord::Dict->String->Double->Dict
addWord d [] f =  d
addWord [] w@(l:[]) f =  [Node (Label l f 1) []] 
addWord [] w@(l:ls) f =  [Node (Label l notWord $ 1) $ addWord [] ls f]
addWord d@(t:ts) w@(l:[]) f =  case compare (ch $ lt) l of
                                 -- if equal increment and set the frequency because it's a word
                                 EQ -> [Node (Label l f $ 1 + count lt) (subForest t)] ++ ts
                                 -- character is bigger so add to the others
                                 LT -> [t] ++ addWord ts w f
                                 -- character is smaller so make a new node in front of the others
                                 GT -> [Node (Label l f $ 1) []] ++ d
                            where
                                lt = rootLabel t
addWord d@(t:ts) w@(l:ls) f =  case compare (ch $ lt) l of
                                 -- if equal increment, but don't change frequency, it's not the end of  word
                                 EQ -> [Node (Label l (logFreq lt) $ 1 + count lt) (addWord (subForest t) ls f)] ++ ts
                                 -- c is bigger so add to the others
                                 LT -> [t] ++ addWord ts w f
                                 -- c is smaller so make a new node in front of the others
                                 GT -> [Node (Label l notWord $ 1) $ addWord [] ls f] ++ d
                            where
                                lt = rootLabel t

-- This has to allow for words like "caT" where the T is cipher text and so the T can be any
-- plain text character
isWord::Dict->String->Bool
isWord _ [] = False
isWord [] _ = False
isWord dd@(t:ts) chs@(c:[]) = case compare (ch $ lt) pc of
                                     EQ -> if isCipherText c then (L.or $ L.map (labelIsWord . rootLabel) dd ) else labelIsWord $ lt
                                     LT -> isWord ts chs
                                     GT -> False
                              where
                                    lt = rootLabel t
                                    -- To force the comparison to be true if it's a cipher text character
                                    pc = if isCipherText c then ch lt else c
isWord dd@(t:ts) chs@(c:cs) = case compare (ch $ lt) pc of
                                     EQ -> if isCipherText c then (L.or $ L.map (\t-> isWord (subForest t) cs) dd) else isWord (subForest t) cs
                                     LT -> isWord ts chs
                                     GT -> False
                              where
                                    lt = rootLabel t
                                    -- To force the comparison to be true if it's a cipher text character
                                    pc = if isCipherText c then ch lt else c

-- This has to allow for words like "caT" where the T is cipher text and so the T can be any
-- plain text character
wordLogFreq::Dict->String->Double
wordLogFreq _ [] = notWord
wordLogFreq [] _ = notWord
wordLogFreq dd@(t:ts) chs@(c:[]) = case compare (ch $ lt) pc of
                                     EQ -> if isCipherText c then (L.sum $ L.map (logFreq . rootLabel) dd ) else logFreq $ lt
                                     LT -> wordLogFreq ts chs
                                     GT -> notWord
                              where
                                    lt = rootLabel t
                                    -- To force the comparison to be true if it's a cipher text character
                                    pc = if isCipherText c then ch lt else c
wordLogFreq dd@(t:ts) chs@(c:cs) = case compare (ch $ lt) pc of
                                     EQ -> if isCipherText c then (L.sum $ L.map (\t-> wordLogFreq (subForest t) cs) dd) else wordLogFreq (subForest t) cs
                                     LT -> wordLogFreq ts chs
                                     GT -> notWord
                              where
                                    lt = rootLabel t
                                    -- To force the comparison to be true if it's a cipher text character
                                    pc = if isCipherText c then ch lt else c

-- Work out a list of words and frequencies from a Tree (and a Dict).
sortedToWords :: Tree Label -> [(Double, String)]
sortedToWords (Node l []) = if labelIsWord l then [(logFreq l, Label.showChar l)] else []
sortedToWords (Node l ks) = sortWith (fst) $ L.map (\(x, s) -> (x, Label.showChar l ++ s)) $ L.concatMap sortedToWords ks



