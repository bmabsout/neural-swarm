{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Brain(module Brain,module SizedL) where

import Convenience
import Data.Proxy
import Numeric.FastMath()
import GHC.TypeLits
import Control.Lens hiding (transform,cons)
import SizedL
import qualified Data.Vector.Storable as V
import           Foreign.Storable.Tuple()

sigmoid :: Floating a => a -> a
sigmoid n = 1/(1 + exp (-n))

newtype Brain ins outs w = Brain { unBrain :: Weights w -> Inputs ins -> Outputs outs }

type B outs ins w = Brain ins outs w

type Weights w = S w Double
type Inputs w = S w Double
type Outputs w = S w Double

type Restorer enabled all = Weights enabled -> Weights all

type BrainBox ins outs w n = (Brain ins outs w, Weights w, Restorer w n)

boxWeights :: Lens' (BrainBox ins outs w n) (Weights w)
boxWeights = lens (\(_,w,_) -> w) (\(a,b,c) w -> (a,w,c))

boxRestorer :: Lens' (BrainBox ins outs w n) (Restorer w n)
boxRestorer = lens (\(_,_,r) -> r) (\(a,b,c) r -> (a,b,r))

infixr 5 \>
(\>) :: _ => B b a w1 -> B c b w2 -> B c a (w1+w2)
(\>) (Brain feed1) (Brain feed2) =
    splitSized &. (\(s1,s2) -> feed1 s1 &. feed2 s2) & Brain

infixl 5 #>
(#>) :: _ => (Brain ins1 outs1 w1, Weights (w2+n), Weights w1, Restorer w1 y)
             -> b2 outs1 outs2 w2
             -> (Brain ins1 outs2 (w1 + NumWeightsOut b2 w2),
                 Weights n,
                 Weights (w1 + NumWeightsOut b2 w2),
                 Weights (w1 + NumWeightsOut b2 w2) -> Weights (y+w2))
(#>) (accumBrain, win, wbefores, recreator) tempBrain =
    (accumBrain \> brain, rest, joinSized wbefores wafters, recreator2)
  where
    recreator2 weights = joinSized (recreator w1) (realSecondWeights tempBrain w2 wbrain)
      where (w1,w2) = splitSized weights

    (wbrain,rest) = splitSized win
    (brain,wafters) = disabler tempBrain wbrain

initBrain :: Weights w -> (Brain ins ins 0, Weights w, Weights 0,Restorer 0 0)
initBrain ws = (Brain (\_ inp -> inp), ws, empty, id)

buildBrain :: _ => (Brain ins outs w, Weights 0, Weights w, Restorer w n) ->
                    BrainBox ins outs w n
buildBrain (brain,_, uneatenWeights, weightRebuilder) = (brain, uneatenWeights, weightRebuilder)

applyBeforeBox (brain,weights,_) f = f brain weights

infixr 6 ><
(><) :: _ => B out1 in1 w1 -> B out2 in2 w2 -> B (out1+out2) (in1+in2) (w1+w2)
(><) (Brain feed1) (Brain feed2) = Brain (\weights inputs ->
    let (w1,w2) = splitSized weights
        (i1,i2) = splitSized inputs
    in joinSized (feed1 w1 i1) (feed2 w2 i2))


stronglyConnected :: _ => B outs ins (ins*outs)
stronglyConnected = Brain (\weights inputs ->
  chunkMap (sZipWith (*) inputs &. ssum &. sigmoid) weights)

shared :: _ => B outs ins w -> B (outs*n) (ins*n) w
shared (Brain feed) = Brain (\weights inputs ->
    transform (feed weights) inputs)

recurrent :: forall n ins shared w . _ => Brain (ins+shared) shared w -> Brain (ins*n) shared w
recurrent (Brain feed) = Brain newFeed
  where
    newFeed weights inputs = sfoldl' (\acc input -> feed weights (joinSized acc input)) (sreplicate 0) feedList
      where
        feedList :: _ => Sized n (Weights ins)
        feedList = chunkMap id inputs

infixr 6 >!<
(>!<) :: _ => B out1 in1 w -> B out2 in2 w -> B (out1+out2) (in1+in2) w
(>!<) (Brain feed1) (Brain feed2) = Brain (\weights inputs ->
    let (i1,i2) = splitSized inputs
    in joinSized (feed1 weights i1) (feed2 weights i2))

biased :: _ => B outs ins ((ins+1)*outs)
biased = Brain (\weights inputs -> (unBrain stronglyConnected) weights (scons 1 inputs))

randWeights :: K n => Double -> Weights n
randWeights = randomS (-1,1)

newtype Disable ins outs w = Disable (Brain ins outs w)

type family NumWeightsOut a (w::Nat) where
  NumWeightsOut Brain w = w
  NumWeightsOut Disable w = 0

type family IfEnabled brain a b where
  IfEnabled Brain a _b = a
  IfEnabled Disable _a b = b

class BrainDisabler a where
  disabler :: (KnownNat w) => a ins outs w -> Weights w ->
                               (Brain ins outs (NumWeightsOut a w), Weights (NumWeightsOut a w))
  realSecondWeights :: (KnownNat w) => a ins outs w -> Weights (NumWeightsOut a w) ->
                                        Weights w -> Weights w

instance BrainDisabler Brain where
  disabler b w = (b, w)
  realSecondWeights b w _ = w

instance BrainDisabler Disable where
  disabler (Disable (Brain feed)) weights = (Brain (\_ -> feed weights), empty)
  realSecondWeights b _ w = w
