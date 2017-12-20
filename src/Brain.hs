{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Brain(module Brain,module SizedL) where

import Convenience
import Numeric.FastMath()
import GHC.TypeLits
import Control.Lens hiding (transform,cons)
import SizedL
import           Foreign.Storable.Tuple()

sigmoid :: Floating a => a -> a
sigmoid n = 1/(1 + exp (-n))

relu :: (Ord a,Floating a) => a -> a
relu = max 0

leakyRelu :: (Ord a, Floating a) => a -> a
leakyRelu z = max (0.01*z) z

newtype Brain ins outs w f = Brain { unBrain :: Weights w f -> Inputs ins f -> Outputs outs f }

type B outs ins w f = Brain ins outs w f

type Weights w f = S w f
type Inputs w f = S w f
type Outputs w f = S w f

type Restorer enabled all f = Weights enabled f -> Weights all f

type BrainBox ins outs w n f = (Brain ins outs w f, Weights w f, Restorer w n f)

boxWeights :: Lens' (BrainBox ins outs w n f) (Weights w f)
boxWeights = lens (\(_,w,_) -> w) (\(a,_,c) w -> (a,w,c))

infixr 5 \>
(\>) :: _ => B b a w1 f -> B c b w2 f -> B c a (w1+w2) f
(\>) (Brain feed1) (Brain feed2) =
    splitSized &. (\(s1,s2) -> feed1 s1 &. feed2 s2) & Brain

infixl 5 #>
(#>) :: _ => (Brain ins1 outs1 w1 f, Weights (w2+n) f, Weights w1 f, Restorer w1 y f)
             -> b2 outs1 outs2 w2 f
             -> (Brain ins1 outs2 (w1 + NumWeightsOut b2 w2) f,
                 Weights n f,
                 Weights (w1 + NumWeightsOut b2 w2) f,
                 Weights (w1 + NumWeightsOut b2 w2) f -> Weights (y+w2) f)
(#>) (accumBrain, win, wbefores, recreator) tempBrain =
    (accumBrain \> brain, rest, joinSized wbefores wafters, recreator2)
  where
    recreator2 weights = joinSized (recreator w1) (realSecondWeights tempBrain w2 wbrain)
      where (w1,w2) = splitSized weights

    (wbrain,rest) = splitSized win
    (brain,wafters) = disabler tempBrain wbrain

initBrain :: Weights w f -> (Brain ins ins 0 f, Weights w f, Weights 0 f,Restorer 0 0 f)
initBrain ws = (Brain (\_ inp -> inp), ws, empty, id)

buildBrain :: _ => (Brain ins outs w f, Weights 0 f, Weights w f, Restorer w n f) ->
                    BrainBox ins outs w n f
buildBrain (brain,_, uneatenWeights, weightRebuilder) = (brain, uneatenWeights, weightRebuilder)

applyBeforeBox :: (t1, t2, t3) -> (t1 -> t2 -> t) -> t
applyBeforeBox (brain,weights,_) f = f brain weights

infixr 6 ><
(><) :: _ => B out1 in1 w1 f -> B out2 in2 w2 f -> B (out1+out2) (in1+in2) (w1+w2) f
(><) (Brain feed1) (Brain feed2) = Brain (\weights inputs ->
    let (w1,w2) = splitSized weights
        (i1,i2) = splitSized inputs
    in joinSized (feed1 w1 i1) (feed2 w2 i2))

infixr 6 >$<
(>$<) :: _ => B out1 ins w1 f -> B out2 ins w2 f -> B (out1+out2) ins (w1+w2) f
(>$<) (Brain feed1) (Brain feed2) = Brain (\weights inputs ->
    let (w1,w2) = splitSized weights
    in joinSized (feed1 w1 inputs) (feed2 w2 inputs))

stronglyConnected :: _ => B outs ins (ins*outs) f
stronglyConnected = Brain (\weights inputs ->
  chunkMap (sZipWith (*) inputs &. ssum &. tanh) weights)

shared :: _ => B outs ins w f -> B (outs*n) (ins*n) w f
shared (Brain feed) = Brain (\weights inputs ->
    transform (feed weights) inputs)

recurrent :: forall n ins shared w f . _ => Brain (ins+shared) shared w f -> Brain (ins*n) shared w f
recurrent (Brain feed) = Brain newFeed
  where
    newFeed weights inputs = sfoldl' (\acc input -> feed weights (joinSized acc input)) (sreplicate 0) feedList
      where
        feedList :: _ => Sized n (Weights ins f)
        feedList = chunkMap id inputs

infixr 6 >!<
(>!<) :: _ => B out1 in1 w f -> B out2 in2 w f -> B (out1+out2) (in1+in2) w f
(>!<) (Brain feed1) (Brain feed2) = Brain (\weights inputs ->
    let (i1,i2) = splitSized inputs
    in joinSized (feed1 weights i1) (feed2 weights i2))

biased :: _ => B outs ins ((ins+1)*outs) f
biased = Brain (\weights inputs -> (unBrain stronglyConnected) weights (scons 1 inputs))

randWeights :: (K n, RealFloat f) => f -> Weights n f
randWeights = randomS (-0.01,0.01)

newtype Disable ins outs w f = Disable (Brain ins outs w f)

type family NumWeightsOut a (w::Nat) where
  NumWeightsOut Brain w = w
  NumWeightsOut Disable w = 0

type family IfEnabled brain a b where
  IfEnabled Brain a _b = a
  IfEnabled Disable _a b = b

class BrainDisabler a where
  disabler :: (KnownNat w) => a ins outs w f -> Weights w f ->
                               (Brain ins outs (NumWeightsOut a w) f, Weights (NumWeightsOut a w) f)
  realSecondWeights :: (KnownNat w) => a ins outs w f -> Weights (NumWeightsOut a w) f ->
                                        Weights w f -> Weights w f

instance BrainDisabler Brain where
  disabler b w = (b, w)
  realSecondWeights _ w _ = w

instance BrainDisabler Disable where
  disabler (Disable (Brain feed)) weights = (Brain (\_ -> feed weights), empty)
  realSecondWeights _ _ w = w

