module Crib (
    Crib(..)
    , emptyCrib
    , makeCrib
    , addCrib
    , applyCrib
    , consistant
    , alphabet
    , Crib.lookup
    , Crib.insert
) where


import Data.Monoid()
import Data.Map as M
import Data.List as L


-- | A crib is a map from cipher text characters to a list of potential plain text characters
-- If the map os empty it needs to return a full alphabet
-- This can be done with the findWithDefault function
data  Crib = Crib {   cDefault::[Char]
                    , cMap :: (Map Char [Char])
                  } deriving (Show)

alphabet = "abcdefghijklmnopqrstuvwxyz"

emptyCrib :: Crib
emptyCrib = Crib alphabet empty

instance Monoid Crib where
    mempty = emptyCrib
    (Crib d1 m1) `mappend` (Crib d2 m2) = Crib (d1 `intersect` d2) (m1 `M.union` m2)

-- | Check to see if two cribs are consistant
consistant :: Crib -> Crib -> Bool
consistant (Crib _ m1) (Crib _ m2) = M.foldWithKey (\ct _ acc -> acc && (m1!ct == m2!ct)) True $ m1 `M.intersection` m2

-- | Get the list of potential plain text characters
lookup :: Crib -> Char -> [Char]
lookup (Crib d cb) c = findWithDefault d c cb

-- | Insert a character into a crib
insert :: Crib -> Char -> Char -> Crib
insert (Crib ds cb) k c = Crib (L.filter (/= c) ds) (M.insert k [c] cb)

-- | Makes a crib from a cipher text, palin text pair
makeCrib :: String -> String -> Crib
makeCrib ct pt = L.foldl (\cb (p,c) -> Crib.insert cb c p) emptyCrib $ zip pt ct

-- | Adds a crib to an existing crib - could use the monoid here - makeCrib and mappend
-- the new data overwrites the one in the crib if there's a conflict
addCrib :: Crib -> String -> String -> Crib
addCrib cbIn ct pt = L.foldl (\cb (p,c) -> Crib.insert cb c p) cbIn $ zip pt ct

-- | Applies a crib to a cipher text (ie. turns some of the letters to lower case)
--   The letter has to be in the crib map.
applyCrib :: Crib -> String -> String
applyCrib cb ct = L.map (\c ->  let pt = Crib.lookup cb c in                       
                                if (length pt == 1) then (head pt) else c
                        ) ct



