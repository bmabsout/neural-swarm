{-# LANGUAGE KindSignatures#-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module SizedL (module SizedL) where

import Data.Proxy
import GHC.TypeLits
import Convenience
import Data.List
import Data.List.Split
import qualified Data.Kind as K
import qualified Data.Vector.Storable as V

newtype Sized (n::Nat) a = Sized [a] deriving (Eq,Functor,Foldable)

instance Show a => Show (Sized n a) where
    show v = "mkN " ++ (fromSized v &> show & unwords) ++ "\n"

type S n a = Sized n a

type K n = KnownNat n

siterateN :: forall a n . (K n) => (a -> a) -> a -> S n a
siterateN f a = iterate f a & take size &  Sized
  where size = typeNum (Proxy :: Proxy n)

sfoldl' :: (b -> a -> b) -> b -> S n a -> b
sfoldl' f e (Sized v) = foldl' f e v


sunsafe :: _ => ([a] -> b) -> S n a -> b
sunsafe f (Sized v) = f v
{-# INLINE sunsafe #-}

ssum :: (Num a) => S n a -> a
ssum = sunsafe sum

shead :: S n a -> a
shead = sunsafe head

stail :: S (n+1) a -> S n a
stail (Sized l) = (Sized (tail l))

replaceAt :: forall m n a. _ => Proxy (m+1) -> Sized (m+n+1) a -> a -> Sized (m+1+n) a
replaceAt index list e = joinSized a (scons e (stail b))
    where
        (a,b) = splitSized list :: (Sized m a,Sized (n+1) a)

sreplicate :: forall n a . (K n) => a -> S n a
sreplicate a = replicate (typeNum (Proxy :: Proxy n)) a & Sized

sizedMap :: S n a -> (a -> b) -> S n b
sizedMap (Sized v) f = map f v & Sized

sconcatMap :: _ => (a -> S n b) -> S m a -> S (n*m) b
sconcatMap f (Sized v) = concatMap (f &. fromSized) v & Sized

sconcat :: _ => S m (S n a) -> S (m*n) a
sconcat l = l &> fromSized & concat & Sized

transform :: forall z a b n m . (K n) =>
                 (S n a -> S m b) -> S (n*z) a -> S (m*z) b
transform f (Sized v) = chunksOf n v &> (Sized &. f &. fromSized) & concat & Sized
  where n = typeNum (Proxy :: Proxy n)

chunkMap :: forall a n chunkSize b .
            (K chunkSize) =>
              (S chunkSize a -> b) -> S (chunkSize*n) a -> S n b
chunkMap f (Sized big) = big & chunksOf chunkSize &> (Sized &. f) & Sized
    where chunkSize = typeNum (Proxy :: Proxy chunkSize)

generate :: forall a i n. (K n,Num i) => (i -> a) -> S n a
generate f = [1::Int ..] & take (typeNum (Proxy :: Proxy n)) &> (fromIntegral &. f) & Sized

randomS :: (RealFloat a,K n) => (a,a) -> a -> S n a
randomS range seed = generate ((+seed) &. pseudoRand range)

joinSized :: S n a -> S m a -> S (n+m) a
joinSized (Sized v1) (Sized v2) = Sized (v1 ++ v2)

splitSized :: forall a n m . (K n) => S (n+m) a -> (S n a,S m a)
splitSized (Sized v) = splitAt (typeNum (Proxy :: Proxy n)) v
                        & (\(v1,v2) -> (Sized v1,Sized v2))

sZipWith :: _ => (a -> b -> c) -> S n a -> S n b -> S n c
sZipWith f (Sized v1) (Sized v2) = Sized (zipWith f v1 v2)
{-# INLINE sZipWith #-}

empty :: S 0 a
empty = Sized []

singleton :: a -> S 1 a
singleton a = Sized [a]

scons :: a -> S n a -> S (n+1) a
scons e (Sized vec) = Sized (e : vec)

snoc :: S n a -> a -> S (n+1) a
snoc (Sized vec) e = Sized (vec ++ [e])

sreverse :: S n a -> S n a
sreverse (Sized l) = Sized (reverse l)

fromSized :: S n a -> [a]
fromSized (Sized v) = v

toList :: S n a -> [a]
toList = fromSized

toVec :: (V.Storable a) => S n a -> V.Vector a
toVec (Sized l) = V.fromList l

fromVec :: (V.Storable a,K n) => V.Vector a -> S n a
fromVec v = v & V.toList & fromList

fromList :: forall n a . K n => [a] -> Sized n a
fromList l = Sized (l & take (typeNum (Proxy :: Proxy n)))

typeNum :: forall f n b . (K n,Num b) => f n -> b
typeNum _ = fromIntegral $ natVal (Proxy :: Proxy n)

ssize :: forall a n b . (K n,Num b) => Sized n a -> b
ssize _ = typeNum (Proxy :: Proxy n)

class Make n a r | r -> a where
    make :: Sized n a -> r

instance Make n a (Sized n a) where
    make x = sreverse x
    {-# INLINE make #-}

instance (Make (n+1) a r) => Make n a (a -> r) where
    make acc a = make (a `scons` acc)
    {-# INLINE make #-}

mkN :: (Make 1 a r) => a -> r
mkN = make empty


data HList (ts :: [K.Type]) :: K.Type where
  Nil  :: HList '[]
  (:>) :: t -> HList ts -> HList (t ': ts)
infixr 5 :>

type family AllC c (xs :: [a]) :: K.Constraint where
  AllC c '[]       = ()
  AllC c (x ': xs) = (c x, AllC c xs)

hmap :: forall c ts. AllC c ts => (forall x. c x => x -> x) -> HList ts -> HList ts
hmap f Nil       = Nil
hmap f (x :> xs) = f x :> hmap @c f xs

hmap' :: forall c ts r. AllC c ts => (forall x. c x => x -> r) -> HList ts -> [r]
hmap' f Nil       = []
hmap' f (x :> xs) = f x : hmap' @c f xs


instance AllC Show ts => Show (HList ts) where
  show xs = "[" ++ intercalate ", " (hmap' @Show show xs) ++ "]"
-- func :: HList (k :: Nat) ->


-- test :: [Int]
-- test = hmap' @KnownNat natVal cheese


test = [natVal (Proxy @3), natVal (Proxy @4)]
