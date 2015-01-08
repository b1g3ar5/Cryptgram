-- Simple Fair back-tracking monad
-- Based on the Scheme code book-si, `Stream implementation, with incomplete'
-- as of Feb 18, 2005

-- $Id: FBackTrack.hs,v 1.4 2005/08/25 20:33:55 oleg Exp oleg $

module FBackTrack (
    Stream(..)
    , runM
)where

import Control.Applicative
import Control.Monad

data Stream a = Nil | One a | Choice a (Stream a) | Incomplete (Stream a)

instance Functor Stream where
  fmap _ Nil = Nil
  fmap f (One a) = One (f a)
  fmap f (Choice a s) = Choice (f a) (fmap f s)
  fmap f (Incomplete s) = Incomplete (fmap f s)

instance Applicative Stream where
  pure = One
  (<*>) = ap
  
instance Monad Stream where
  return = One

  Nil          >>= _ = Nil
  One a        >>= f = f a
  Choice a r   >>= f = f a `mplus` (Incomplete (r >>= f))
  Incomplete i >>= f = Incomplete (i >>= f)

instance Alternative Stream where
    empty = Nil
    (<|>) = mplus

instance MonadPlus Stream where
  mzero = Nil

  mplus Nil r'          = Incomplete r'
  mplus (One a) r'      = Choice a r'
  mplus (Choice a r) r' = Choice a (mplus r' r) -- interleaving!
  --mplus (Incomplete i) r' = Incomplete (mplus i r')
  mplus r@(Incomplete i) r' = 
      case r' of
	      Nil         -> r
	      One b       -> Choice b i
	      Choice b r'' -> Choice b (mplus i r'')
	      -- Choice _ _ -> Incomplete (mplus r' i)
	      Incomplete j -> Incomplete (mplus i j)


-- run the Monad, to a specific depth
runM :: Maybe Int -> Stream a -> [a]
runM _ Nil = []
runM _ (One a) = [a]
runM d (Choice a r) = a : (runM d r)
runM (Just 0) (Incomplete _) = []	-- exhausted depth
runM d (Incomplete r) = runM d' r
    where d' = d >>= (return . (+ (-1)))

-- Don't try the following with the regular List monad or List comprehension!
-- That would diverge instantly: all `i', `j', and `k' are infinite
-- streams

pythagorean_triples :: MonadPlus m => m (Int,Int,Int)
pythagorean_triples =
    let number = (return 0) `mplus` (number >>= return . succ) in
    do
    i <- number
    guard $ i > 0
    j <- number
    guard $ j > 0
    k <- number
    guard $ k > 0
    guard $ i*i + j*j == k*k
    return (i,j,k)

test :: [(Int, Int, Int)]
test = take 7 $ runM Nothing pythagorean_triples

-- The following code is not in general MonadPlus: it uses Incomplete
-- explicitly. But it supports left recursion! Note that in OCaml, for example,
-- we _must_ include that Incomplete data constructor to make
-- the recursive definition well-formed.  
-- The code does *not* get stuck in the generation of primitive tuples
-- like (0,1,1), (0,2,2), (0,3,3) etc.
pythagorean_triples' :: Stream (Int, Int, Int)
pythagorean_triples' =
    let number = (Incomplete number >>= return . succ) `mplus` return 0  in
    do
    i <- number
    j <- number
    k <- number
    guard $ i*i + j*j == k*k
    return (i,j,k)

test' :: [(Int, Int, Int)]
test' = take 27 $ runM Nothing pythagorean_triples'


-- a serious test of left recursion (due to Will Byrd)
flaz :: (Num a, Eq a) => a -> Stream a
flaz x = Incomplete (flaz x) `mplus` (Incomplete (flaz x) `mplus`
				      if x == 5 then return x else mzero)

test_flaz :: [Int]
test_flaz = take 15 $ runM Nothing (flaz 5)

-- `Don't care non-determinism'
--  (once m) non-deterministically returns one of the results produced
--  by m, disregarding all the rest (or fails when m fails)

class MonadPlus m => MonadPlusOnce m where
    once :: m a -> m a

instance MonadPlusOnce Stream where
    once Nil = Nil
    once x@One{} = x
    once (Choice a r)   = One a         -- disregard the rest
    once (Incomplete r) = Incomplete (once r)
