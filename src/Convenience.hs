{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Convenience
(module Convenience,module Data.Function,module Data.Proxy) where

import           Data.Function
-- import qualified Data.Set as S
import           Data.List
import           Data.Ord
import           Control.Parallel
import           Control.Parallel.Strategies
import           Numeric.FastMath()
import qualified Data.Vector.Storable as V
import           Foreign.Storable.Tuple()
import           Data.Proxy


infixl 2 &.
(&.) :: (a -> b) -> (b -> c) -> a -> c
(&.) = flip (.)
{-# INLINE (&.) #-}

infixl 3 &.>
(&.>) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
(&.>) g f = fmap f . g
{-# INLINE (&.>) #-}

infixl 1 &>
(&>) :: Functor f => f a -> (a -> b) -> f b
(&>) = flip (<$>)
{-# INLINE (&>) #-}

infixl 1 &!>
(&!>) :: NFData b => [a] -> (a -> b) -> [b]
(&!>) l a = parMap rdeepseq a l
{-# INLINE (&!>) #-}

ifd :: Bool -> (a->a) -> (a->a)
ifd b f = if b then f else id

ifz :: (a -> Bool) -> (a -> a) -> a -> a
ifz b f a = if b a then f a else a

if' :: a -> a -> Bool -> a
if' x _ True  = x
if' _ y False = y

getIxs :: Integral a => [b] -> [a] -> [b]
getIxs l indices = indices & sort & mapAccumL accumate (0,l) & snd
    where accumate (oldI,currL) i = ((i,dropped),head dropped)
              where dropped = genericDrop (i-oldI) currL
ofLength :: Integral b => b -> [a] -> Bool
ofLength 0 []     = True
ofLength 0 _      = False
ofLength _ []     = False
ofLength k (_:xs) = ofLength (k - 1) xs

subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs = let l = length xs
                          in if n>l then [] else subsequencesBySize xs !! (l-n)
 where
   subsequencesBySize [] = [[[]]]
   subsequencesBySize (y:ys) = let next = subsequencesBySize ys
                             in zipWith (++) ([]:next) (fmap (fmap (y:)) next `mappend` [[]])

transposeCut :: [[a]] -> [[a]]
transposeCut [] = []
transposeCut l
    | any null l = []
    | otherwise = (l &> head) : transposeCut (l &> tail)

window :: (Integral b) => ([a] -> c) -> b -> [a] -> [c]
window f n l = tails l & take (fromIntegral n)
                       & transposeCut
                       &> f

readTwo :: (Read a, Read b) => IO (a,b)
readTwo = getLine &> words &. \(a:b:_) -> (read a, read b)

readThree :: (Read a, Read b, Read c) => IO (a,b,c)
readThree = getLine &> words &. \(a:b:c:_) -> (read a, read b, read c)

readWords :: Read a => IO [a]
readWords = getLine &> words &.> read

ana :: (Eq a,Num a) => a -> (b -> (t,b)) -> b -> [t]
ana !siz f = go siz
  where go 1 acc = [f acc & fst]
        go n acc = stay : go (n-1) next
          where (stay,next) = f acc

apply :: (Num a, Ord a) => a -> (t -> t) -> t -> t
apply !n !f !beg = go beg n
  where go !acc !i
            | i <= 0    = acc
            | otherwise = go (f acc) (i-1)


ring :: RealFrac a => (a, a) -> a -> a
ring (a,b) n
    | a == b = a
    | otherwise = a + go (b-a) (n-a)
    where
        go i x
            | x >= i = x - i* fromIntegral (floor (x/i) :: Int)
            | x < 0  = x + i* fromIntegral (ceiling (-x/i) :: Int)
            | otherwise = x


pseudoRand :: RealFloat a => (a,a) -> a -> a
pseudoRand (a,b) seed = ring (a,b) ((sin (seed * ring (20,30) seed) + 1) * (newB-newA) /2 + newA)
    where newA = (a+b)/2 + (a - (a+b)/2)*10
          newB = (a+b)/2 + (b - (a+b)/2)*10

pseudoRands :: RealFloat a => (a,a) -> a -> [a]
pseudoRands (a,b) seed = iterate (pseudoRand (a,b)) seed & tail

newtype Vec a = Vec (a,a) deriving (Eq, Show,V.Storable)

instance Num a => Num (Vec a) where
    Vec (a,b) + Vec (a2,b2) = Vec (a+a2,b+b2)
    Vec (a,b) - Vec (a2,b2) = Vec (a-a2,b-b2)
    Vec (a,b) * Vec (a2,b2) = Vec (a*a2,b*b2)
    abs (Vec (a,b)) = Vec (abs a,abs b)
    signum (Vec (a,b)) = Vec (signum a,signum b)
    fromInteger a = Vec (fromInteger a,fromInteger a)

vecToList :: Vec a -> [a]
vecToList (Vec (x,y)) = [x,y]

listToVec :: [a] -> Vec a
listToVec [x,y] = Vec (x,y)

dot :: Num a => Vec a -> Vec a -> a
dot (Vec (x1,y1)) (Vec (x2,y2))= x1*x2 + y1*y2

magVsq :: Num a => Vec a -> a
magVsq (Vec (x,y)) = x*x + y*y

distsq :: Num a => Vec a -> Vec a -> a
distsq !p1 !p2 = magVsq (p1-p2)

rotateVec :: Floating a => Vec a -> a -> Vec a
rotateVec (Vec (x,y)) o = Vec (x * sin o, y*  cos o)

dist :: Floating b => Vec b -> Vec b -> b
dist !p1 !p2 = distsq p1 p2 & sqrt

instance Fractional a => Fractional (Vec a) where
  fromRational a = Vec (fromRational a,fromRational a)
  (/) (Vec (ax,ay)) (Vec (bx,by)) = Vec (ax/bx,ay/by)

fromScalar :: a -> Vec a
fromScalar a = Vec (a,a)

qsort :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
qsort _   []     r = r
qsort _   [x]    r = x:r
qsort cmp (x:xs) r = qpart cmp x xs [] [] r

-- qpart partitions and sorts the sublists
qpart :: (a -> a -> Ordering) -> a -> [a] -> [a] -> [a] -> [a] -> [a]
qpart cmp x [] rlt rge r =
    -- rlt and rge are in reverse order and must be sorted with an
    -- anti-stable sorting
    rqsort cmp rlt (x:rqsort cmp rge r)
qpart cmp x (y:ys) rlt rge r =
    case cmp x y of
        GT -> qpart cmp x ys (y:rlt) rge r
        _  -> qpart cmp x ys rlt (y:rge) r

-- rqsort is as qsort but anti-stable, i.e. reverses equal elements
rqsort :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
rqsort _   []     r = r
rqsort _   [x]    r = x:r
rqsort cmp (x:xs) r = rqpart cmp x xs [] [] r

rqpart :: (a -> a -> Ordering) -> a -> [a] -> [a] -> [a] -> [a] -> [a]
rqpart cmp x [] rle rgt r =
    qsort cmp rle (x:qsort cmp rgt r)
rqpart cmp x (y:ys) rle rgt r =
    case cmp y x of
        GT -> rqpart cmp x ys rle (y:rgt) r
        _  -> rqpart cmp x ys (y:rle) rgt r

qsortOn :: (Ord a) => (b -> a) -> [b] -> [b]
qsortOn f l = qsort (comparing f) l []


lerp :: Num a => a -> a -> a -> a
lerp a b n = (b - a)*n + a

class Default a where
  auto :: a
