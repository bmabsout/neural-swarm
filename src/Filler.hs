{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
module Filler(Filler, fillerSimulatorInstance, fillerNeuralInstance) where

import Brain
import Convenience
import Minimizer
import Simulator
import Graphics.Gloss

type Updater a = [Vec a] -> Vec a -> [Vec a]

data Filler a = Filler [Vec a] (Vec a, a) (Updater a)

fillerSimulatorInstance :: RealFloat a => Simulator (Filler a) a
fillerSimulatorInstance = Simulator simRender simStep simCost mainState
    where
        simRender (Filler (points, goal, _)) =
            points &> (\p -> circleSolid 8 & vecTranslate p
                                           & color white)
                   & (:) (circle () & vecTranslate (computeGoal goal)
                                    & color red)
                   & pictures
            where vecTranslate (Vec (x,y)) = translate (realToFrac x) (realToFrac y)

        simStep (Filler points goal updater) = Filler (zipWith (+) points vecs) goal updater
            where vecs = updater points goal

        simCost (Filler points goal _) = points &> distsq goal & sum

        mainState = randFillers (100,100) (applyBeforeBox bronx neuralUpdater) 114678

randFillers :: RealFloat a => (a,a) -> Updater a -> a -> Filler a
randFillers numFillersRange updater seed = Filler points (randGoal,0) updater
    where range = (-500,500)
          randGoal = Vec (pseudoRand range seed, pseudoRand range (seed+1))
          numFillers = pseudoRand numFillersRange (seed+2) & floor
          points = zipWith (curry Vec)
                           (pseudoRands range (seed+3))
                           (pseudoRands range (seed+4))
                   & take numFillers

myUpdater points goal = points &> (\p -> (goal - p)/(dist goal p & fromScalar))

neuralUpdater :: (Num a) => Brain a 4 2 w -> Weights w a -> Updater a
neuralUpdater (Brain feed) weights points goal =
    points &> (\p -> vecToSized (p,goal) & feed weights & sizedToVec)
  where
    vecToSized :: (Vec a,Vec a) -> Sized 4 a
    vecToSized (Vec (a,b), Vec (c,d)) = Sized [a,b,c,d]
    sizedToVec :: Num a => Sized 2 a -> Vec a
    sizedToVec (Sized [a,b]) = Vec (lerp -10 10 a, lerp -10 10 b)

fillerNeuralInstance :: RealFloat a => NeuralSim (Filler a) a _ _ _ _
fillerNeuralInstance = NeuralSim fillerSimulatorInstance currentBox randTrainingState neuralStep
    where
        currentBox@(brain,_,_) = bronx
        neuralStep (Filler (a,b,_)) weights = _simStep fillerSimulatorInstance (Filler a b (neuralUpdater brain weights))
        randTrainingState seed weights =
            randFillers (10,10) (neuralUpdater brain weights) (seed+1)
