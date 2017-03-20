{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SizedS (module SizedS) where

import Data.Proxy
import GHC.TypeLits
import Convenience
import Control.Arrow (first)
import Data.Foldable
import qualified Data.Vector.Storable as V
import qualified Data.Sequence as S

newtype Sized (n::Nat) a = Sized (S.Seq a) deriving (Eq,Functor,Foldable)

instance Show a => Show (Sized n a) where
    show v = "mkN " ++ (fromSized v &> show & foldl' (\acc e -> acc ++ " " ++ e) "") ++ "\n"

type S n a = Sized n a

type K n = KnownNat n

siterateN :: forall a n . (KnownNat n) => (a -> a) -> a -> S n a
siterateN f a = S.iterateN size f a &  Sized
  where size = typeNum (Proxy :: Proxy n)

sfoldl' :: (b -> a -> b) -> b -> S n a -> b
sfoldl' f e (Sized v) = foldl' f e v

sunsafe :: _ => (S.Seq a -> b) -> S n a -> b
sunsafe f (Sized v) = f v
{-# INLINE sunsafe #-}

ssum :: (Num a) => S n a -> a
ssum = sunsafe sum

shead :: (1 <= n) => S n a -> a
shead (Sized s) = S.index s 0

sreplicate :: forall n a . (KnownNat n) => a -> S n a
sreplicate a = S.replicate (typeNum (Proxy :: Proxy n)) a & Sized

sizedMap :: S n a -> (a -> b) -> S n b
sizedMap = flip fmap

transform :: forall z a b n m . (KnownNat z,KnownNat n) =>
                 (S n a -> S m b) -> S (n*z) a -> S (m*z) b
transform f (Sized v) =  ana z (S.splitAt n &. first (Sized &. f &. fromSized)) v & foldr (S.><) S.empty & Sized
  where z = typeNum (Proxy :: Proxy z) :: Int
        n = typeNum (Proxy :: Proxy n)

chunkMap :: forall a n chunkSize b .
            (KnownNat n, KnownNat chunkSize) =>
              (S chunkSize a -> b) -> S (chunkSize*n) a -> S n b
chunkMap f (Sized big) = ana n (S.splitAt chunkSize &. first (Sized &. f)) big & fromList
    where chunkSize = typeNum (Proxy :: Proxy chunkSize)
          n = typeNum (Proxy :: Proxy n) :: Int

generate :: forall a i n. (KnownNat n,Num i) => (i -> a) -> S n a
generate f = S.fromFunction (typeNum (Proxy :: Proxy n)) (fromIntegral &. f) & Sized

randomS :: (RealFloat a,KnownNat n) => (a,a) -> a -> S n a
randomS range seed = generate ((+seed) &. pseudoRand range)

joinSized :: S n a -> S m a -> S (n+m) a
joinSized (Sized v1) (Sized v2) = Sized (v1 S.>< v2)

splitSized :: forall a n m . (KnownNat n) => S (n+m) a -> (S n a,S m a)
splitSized (Sized v) = S.splitAt (typeNum (Proxy :: Proxy n)) v
                        & (\(v1,v2) -> (Sized v1,Sized v2))

sZipWith :: _ => (a -> b -> c) -> S n a -> S n b -> S n c
sZipWith f (Sized v1) (Sized v2) = Sized (S.zipWith f v1 v2)
{-# INLINE sZipWith #-}

empty :: S 0 a
empty = Sized (S.empty)

singleton :: a -> S 1 a
singleton a = Sized (S.singleton a)

cons :: a -> S n a -> S (n+1) a
cons e (Sized vec) = Sized (e S.<| vec)

snoc :: S n a -> a -> S (n+1) a
snoc (Sized vec) e = Sized (vec S.|> e)

fromSized :: S n a -> S.Seq a
fromSized (Sized v) = v

toVec :: (V.Storable a) => S n a -> V.Vector a
toVec (Sized s) = s & toList & V.fromList

fromVec :: V.Storable a => V.Vector a -> S n a
fromVec v =  v & V.toList & fromList

fromList :: [a] -> Sized n a
fromList l = Sized (S.fromList l)

typeNum :: forall f n b . (KnownNat n,Num b) => f n -> b
typeNum _ = fromIntegral $ natVal (Proxy :: Proxy n)

ssize :: forall a n b . (KnownNat n,Num b) => Sized n a -> b
ssize _ = typeNum (Proxy :: Proxy n)

class Make n a r | r -> a where
    make :: Sized n a -> r

instance Make n a (Sized n a) where
    make x = x
    {-# INLINE make #-}

instance (Make (n+1) a r) => Make n a (a -> r) where
    make acc a = make (acc `snoc` a)
    {-# INLINE make #-}

mkN :: (Make 1 a r) => a -> r
mkN = make empty
