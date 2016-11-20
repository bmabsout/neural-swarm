{-# LANGUAGE KindSignatures,DataKinds,TypeOperators,ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Brain(module Brain,module SizedL) where

import Convenience
import Data.Proxy
import Numeric.FastMath()
import GHC.TypeLits
import SizedL
import qualified Data.Vector.Storable as V
import           Foreign.Storable.Tuple()

sigmoid :: Floating a => a -> a
sigmoid n = 1/(1 + exp (-n))

newtype Brain t ins outs w = Brain { unBrain :: Weights w t -> Inputs ins t -> Outputs outs t }

type B outs ins t w = Brain t ins outs w

type Weights w t = S w t
type Inputs w t = S w t
type Outputs w t = S w t

infixr 5 \>
(\>) :: _ => B b a t w1 -> B c b t w2 -> B c a t (w1+w2)
(\>) (Brain feed1) (Brain feed2) =
    splitSized &. (\(s1,s2) -> feed1 s1 &. feed2 s2) & Brain

infixr 5 #>
(#>) :: _ => (b1 t ins1 outs1 w1, Weights (w1+w2) t)
             -> b2 t outs1 outs2 w2
             -> (Brain t ins1 outs2 (NumWeightsOut b1 w1 + NumWeightsOut b2 w2), Weights (NumWeightsOut b1 w1 + NumWeightsOut b2 w2) t,
                 Weights (NumWeightsOut b1 w1 + NumWeightsOut b2 w2) t -> Weights (w1+w2) t)
(#>) (brain1,win) brain2 = (bout1 \> bout2, joinSized wout1 wout2, undefined)
    where
      (win1,win2) = splitSized win
      (bout1,wout1) = disabler brain1 win1
      (bout2,wout2) = disabler brain2 win2

infixr 6 ><
(><) :: _ => B out1 in1 t w1 -> B out2 in2 t w2 -> B (out1+out2) (in1+in2) t (w1+w2)
(><) (Brain feed1) (Brain feed2) = Brain (\weights inputs ->
    let (w1,w2) = splitSized weights
        (i1,i2) = splitSized inputs
    in joinSized (feed1 w1 i1) (feed2 w2 i2))


stronglyConnected :: _ => B outs ins t (ins*outs)
stronglyConnected = Brain (\weights inputs ->
  chunkMap (sZipWith (*) inputs &. ssum &. sigmoid) weights)

shared :: _ => Proxy n -> B outs ins t w -> B (outs*n) (ins*n) t w
shared prox (Brain feed) = Brain (\weights inputs ->
    transform prox (feed weights) inputs)

infixr 6 >!<
(>!<) :: _ => B out1 in1 t w -> B out2 in2 t w -> B (out1+out2) (in1+in2) t w
(>!<) (Brain feed1) (Brain feed2) = Brain (\weights inputs ->
    let (i1,i2) = splitSized inputs
    in joinSized (feed1 weights i1) (feed2 weights i2))

biased :: _ => B outs ins t ((ins+1)*outs)
biased = Brain (\weights inputs -> (unBrain stronglyConnected) weights (cons 1 inputs))

randWeights :: (K n,RealFloat a) => a -> Weights n a
randWeights = randomS (-1,1)


newtype Disable t ins outs w = Disable (Brain t ins outs w)

type family NumWeightsOut a (w::Nat) where
  NumWeightsOut Brain w = w
  NumWeightsOut Disable w = 0

type family NegNumWeightsOut a (w::Nat) where
  NegNumWeightsOut Brain w = 0
  NegNumWeightsOut Disable w = w

class BrainDisabler a where
  disabler :: (KnownNat w) => a t ins outs w -> Weights w t ->
                               (Brain t ins outs (NumWeightsOut a w), Weights (NumWeightsOut a w) t)

instance BrainDisabler Brain where
  disabler b w = (b, w)

instance BrainDisabler Disable where
  disabler (Disable (Brain feed)) weights = (Brain (\_ -> feed weights), empty)
