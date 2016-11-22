{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Test(Test, testSimulatorInstance, testNeuralInstance) where

import Brain
import Convenience
import Minimizer
import Simulator
import Graphics.Gloss

type Updater a = [Vec a] -> Vec a -> [Vec a]

newtype Test a = Test ([Vec a],(Vec a, a),Updater a)

testSimulatorInstance :: RealFloat a => Simulator (Test a) a
testSimulatorInstance = Simulator simRender simStep simCost mainState
    where
        simRender (Test (points, goal, _)) =
            points &> (\p -> circleSolid 8 & vecTranslate p
                                           & color white)
                   & (:) (circleSolid 16 & vecTranslate (computeGoal goal)
                                         & color red)
                   & pictures
            where vecTranslate (Vec (x,y)) = translate (realToFrac x) (realToFrac y)

        simStep (Test (points, goal@(loc, angle) , updater)) = Test (zipWith (+) points vecs, (loc, angle+0.1), updater)
            where vecs = updater points (computeGoal goal)

        simCost (Test (points, goal, _)) = points &> distsq (computeGoal goal) & sum

        mainState = randTests (100,100) (applyBeforeBox box neuralUpdater) 114678

computeGoal :: Floating a => (Vec a, a) -> Vec a
computeGoal (loc, angle) = rotateVec (fromScalar 100) angle + loc

randTests :: RealFloat a => (a,a) -> Updater a -> a -> Test a
randTests numTestsRange updater seed = Test (points, (randGoal,0), updater)
    where range = (-500,500)
          randGoal = Vec (pseudoRand range seed, pseudoRand range (seed+1))
          numTests = pseudoRand numTestsRange (seed+2) & floor
          points = zipWith (curry Vec)
                           (pseudoRands range (seed+3))
                           (pseudoRands range (seed+4))
                   & take numTests

myUpdater points goal = points &> (\p -> (goal - p)/(dist goal p & fromScalar))

neuralUpdater :: (Num a) => Brain a 4 2 w -> Weights w a -> Updater a
neuralUpdater (Brain feed) weights points goal =
    points &> (\p -> vecToSized (p,goal) & feed weights & sizedToVec)
  where
    vecToSized :: (Vec a,Vec a) -> Sized 4 a
    vecToSized (Vec (a,b), Vec (c,d)) = Sized [a,b,c,d]
    sizedToVec :: Num a => Sized 2 a -> Vec a
    sizedToVec (Sized [a,b]) = Vec (lerp -10 10 a, lerp -10 10 b)

testNeuralInstance :: RealFloat a => NeuralSim (Test a) a _ _
testNeuralInstance = NeuralSim testSimulatorInstance currentBox randTrainingState
    where
        currentBox@(brain,_,_) = box
        randTrainingState seed weights =
            randTests (100,100) (neuralUpdater brain weights) (seed+1)

box :: _ => BrainBox a _ _ _ _
box = buildBrain (initBrain goodWeights #> biased @4 #> biased @2)

trainBrain :: _ => Brain a _ _ _
trainBrain = biased @4 \> biased @2

goodWeights :: Floating a => Weights 30 a
goodWeights =
    mkN 4.505400891503356 -2.4354178296880287 -1.9151942652956775 2.3659395892432427 1.9105919889749314 4.354794548414795 3.7286799433188467 -3.970191119375574e-2 -3.105466480956899 1.1554450393105569 2.918408954394563 2.1185418291984135 0.3265448402910385 -2.2647199440708423 -0.7762503502073099 3.2045360184426226 -5.772723770101356e-3 4.1435664070479685 8.029700038038773e-2 -3.9179562926897704 4.074062294996835 6.361358300709709 -2.2621857847514684 -5.503326398232776 1.516791704754974 1.3675281870616143 3.2918130781269244 1.277826814964007 0.21200973260688694 -12.99952038472853

