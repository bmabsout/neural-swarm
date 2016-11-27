
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Minimizer(NeuralSim(..), printMinimizer) where

import           Brain
import           Convenience
import           Data.List
import           Data.List.Split
import qualified Data.Vector.Storable     as V
import           GHC.TypeLits
import           Numeric.GSL.Minimization
import           Numeric.GSL.SimulatedAnnealing
import           Numeric.LinearAlgebra.Data
import           Simulator
import           Foreign.C.Types
import           Control.Arrow
type Minimizer c n a = c -> (Weights n a -> a) -> Weights n a -> (Weights n a,Vector a)

data NeuralSim system floating w n = forall ins outs. NeuralSim {
    _simulatorInstance :: Simulator system floating,
    _startBox          :: BrainBox floating ins outs w n,
    _randTrainingState :: floating -> Weights w floating -> system
}


minimizeS :: (KnownNat n) => Minimizer Int n Double
minimizeS iterations cost xi = minimizeV NMSimplex2 0.0000001 iterations (V.replicate (ssize xi) 1) (fromVec &. cost) (toVec xi) & second (toColumns &. (!!1)) & first fromVec

annealing :: (KnownNat n) => (Weights n Double -> String) -> Minimizer CInt n Double
annealing printer iterations cost xi = (simanSolve 123 (ssize xi) anParams xi cost metricDist stepFunction (Just printer), V.empty)
  where anParams = SimulatedAnnealingParams 1000 iterations 1.0 1.0 0.1 1.02 0.001
        metricDist a1 a2 = sZipWith (\x1 x2 -> (x1-x2)*(x1-x2)) a1 a2 & ssum & sqrt
        stepFunction rands stepSize current = rands & fromVec &> (\x -> x*2*stepSize - stepSize) & sZipWith (+) current

networkMinimizer :: (RealFloat b, Integral c) =>
    Minimizer c n b -> NeuralSim a b n w -> ([Vector b],Weights w b, [(b,b)])
networkMinimizer optimizer (NeuralSim simulator startBox randTrainingState) =
  (paths,weightRestorer minimizedWeights, zip (defaultCosts startWeights) (defaultCosts minimizedWeights))
    where
        (startBrain, startWeights, weightRestorer) = startBox
        generalSeed = 23555844363
        optiters = 1000
        simIterRange = (200,600)
        numSystemsPerMinimization = 5
        numMinimizations = 10
        defaultCosts weights = [100,200..1000] &> costOfRun (randTrainingState generalSeed weights)
        costOfRun state iters = simulateN simulator (iters::Int) state & fst
        randIter seed = pseudoRand simIterRange seed & floor
        sseeds = pseudoRands (0,1000) generalSeed & chunksOf numSystemsPerMinimization
                                                  & take numMinimizations
        (minimizedWeights,paths) = mapAccumL minimizeWeightSeeds startWeights sseeds

        minimizeWeightSeeds weights seeds =
            optimizer optiters (costOfFlatPerSeeds seeds) weights

        costOfFlatPerSeeds seeds weights =
            seeds &> randIter
                  &> costOfRun (randTrainingState generalSeed weights)
                  &> (^^2)
                  & sum


printMinimizer :: KnownNat n => NeuralSim a Double n w -> IO ()
printMinimizer neuralSim@(NeuralSim _ (_,_,restorer) _) = do
  mapM_ print a
  print b
  print c
    where (a,b,c) = networkMinimizer minimizeS neuralSim
          minimizeA = annealing (restorer &. show)
