{-# LANGUAGE DataKinds,KindSignatures,TypeOperators,TypeFamilies,ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module SizedV (module SizedV,module Data.Proxy) where

import qualified Data.Vector.Storable as V
import Data.Proxy
import GHC.TypeLits
import Convenience
import Foreign.Storable
import Foreign.Ptr
import qualified Data.Foldable as F

newtype Sized (n::Nat) a = Sized (V.Vector a) deriving Eq

instance (Show a,V.Storable a)=> Show (Sized n a) where
    show v = "mkN " ++ (toList v &> show & unwords)

type S n a = Sized n a

type K n = KnownNat n

siterateN :: forall a n . (V.Storable a,KnownNat n) => (a -> a) -> a -> S n a
siterateN f a = V.iterateN size f a & Sized
  where size = typeNum (Proxy :: Proxy n)

sfoldl' :: (V.Storable a) => (b -> a -> b) -> b -> S n a -> b
sfoldl' f e (Sized v) = V.foldl' f e v


sunsafe :: _ => (V.Vector a -> b) -> S n a -> b
sunsafe f (Sized v) = f v
{-# INLINE sunsafe #-}

ssum :: (V.Storable a,Num a) => S n a -> a
ssum = sunsafe V.sum

shead :: (1 <= n,V.Storable a) => S n a -> a
shead = sunsafe V.unsafeHead

stail :: (1 <= n,V.Storable a) => S (n+1) a -> S n a
stail (Sized l) = (Sized (V.tail l))

sreplicate :: forall n a . (V.Storable a,KnownNat n) => a -> S n a
sreplicate a = V.replicate (typeNum (Proxy :: Proxy n)) a & Sized

sizedMap :: (V.Storable a,V.Storable b) => S n a -> (a -> b) -> S n b
sizedMap (Sized v) f = V.map f v & Sized

sconcatMap :: _ => (a -> S n b) -> S m a -> S (n*m) b
sconcatMap f (Sized v) = V.concatMap (f &. fromSized) v & Sized

transform :: forall z a b n m . (KnownNat z,KnownNat n, V.Storable a, V.Storable b) =>
                 Proxy z -> (S n a -> S m b) -> S (n*z) a -> S (m*z) b
transform prox f (Sized v) = [0 .. z - 1] &> (\i -> V.unsafeSlice (i*n) n v & Sized & f & fromSized)
                                      & V.concat & Sized
  where z = typeNum prox
        n = typeNum (Proxy :: Proxy n)

chunkMap :: forall a n chunkSize b .
            (V.Storable b, V.Storable a,KnownNat n, KnownNat chunkSize) =>
              (S chunkSize a -> b) -> S (chunkSize*n) a -> S n b
chunkMap f (Sized big) =
    generate (\i -> V.unsafeSlice (i*chunkSize) chunkSize big & Sized & f)
    where chunkSize = typeNum (Proxy :: Proxy chunkSize)

generate :: forall a i n. (V.Storable a,KnownNat n,Num i) => (i -> a) -> S n a
generate f = V.generate (typeNum (Proxy :: Proxy n)) (fromIntegral &. f) & Sized

randomS :: (V.Storable a,RealFloat a,KnownNat n) => (a,a) -> a -> S n a
randomS range seed = generate ((+seed) &. pseudoRand range)

joinSized :: V.Storable a => S n a -> S m a -> S (n+m) a
joinSized (Sized v1) (Sized v2) = Sized (v1 V.++ v2)

splitSized :: forall a n m . (KnownNat n,V.Storable a) => S (n+m) a -> (S n a,S m a)
splitSized (Sized v) = V.splitAt (typeNum (Proxy :: Proxy n)) v
                        & (\(v1,v2) -> (Sized v1,Sized v2))

sliceSized ::
  forall k n vSize a . (V.Storable a,KnownNat k,KnownNat n,KnownNat vSize, k+n <= vSize - 1) =>
      Proxy k -> S vSize a -> S n a
sliceSized proxy (Sized vec) = V.unsafeSlice (typeNum proxy) (typeNum (Proxy :: Proxy n)) vec & Sized

sZipWith :: _ => (a -> b -> c) -> S n a -> S n b -> S n c
sZipWith f (Sized v1) (Sized v2) = Sized (V.zipWith f v1 v2)
{-# INLINE sZipWith #-}

empty :: V.Storable a => S 0 a
empty = Sized V.empty

singleton :: V.Storable a => a -> S 1 a
singleton a = Sized (V.singleton a)

cons :: V.Storable a => a -> S n a -> S (n+1) a
cons e (Sized vec) = Sized (V.cons e vec)

snoc :: V.Storable a => S n a -> a -> S (n+1) a
snoc (Sized vec) e = Sized (V.snoc vec e)

toList :: (V.Storable a) => S n a -> [a]
toList (Sized v) = V.toList v

fromSized :: S n a -> V.Vector a
fromSized (Sized v) = v

fromVec :: V.Vector a -> S n a
fromVec = Sized

toVec :: S n a -> V.Vector a
toVec (Sized v) = v

fromList :: V.Storable a => [a] -> Sized n a
fromList l = Sized $ V.fromList l

typeNum :: forall f n b . (KnownNat n,Num b) => f n -> b
typeNum _ = fromIntegral $ natVal (Proxy :: Proxy n)

ssize :: forall a n b . (KnownNat n,Num b) => Sized n a -> b
ssize _ = typeNum (Proxy :: Proxy n)

class Make n a r | r -> a where
    make :: Sized n a -> r

instance Make n a (Sized n a) where
    make x = x
    {-# INLINE make #-}

instance (Make (n+1) a r,V.Storable a) => Make n a (a -> r) where
    make acc a = make (acc `snoc` a)
    {-# INLINE make #-}

mkN :: (V.Storable a,Make 1 a r) => a -> r
mkN = make empty



instance (KnownNat n, Storable a) => Storable (Sized n a) where
  sizeOf _ = typeNum (Proxy :: Proxy n) * sizeOf (undefined:: a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined :: a)
  {-# INLINE alignment #-}
  poke ptr (Sized xs) = F.forM_ [0..typeNum (Proxy :: Proxy n)-1] $ \i ->
    pokeElemOff ptr' i (V.unsafeIndex xs i)
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = Sized <$> V.generateM (typeNum (Proxy :: Proxy n)) (peekElemOff ptr')
    where ptr' = castPtr ptr
  {-# INLINE peek #-}
