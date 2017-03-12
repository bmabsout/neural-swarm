
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Minimizer(module Minimizer) where

import           Brain
import           Convenience
import           Data.List
import           Data.List.Split
import qualified Data.Vector.Storable     as V
import           GHC.TypeLits
import           Control.Lens
import           Numeric.GSL.Minimization
import           Numeric.GSL.SimulatedAnnealing
import           Numeric.LinearAlgebra.Data
import           Simulator
import           Control.Parallel.Strategies
import           Foreign.C.Types
import           Control.Arrow

type Minimizer n = (Weights n -> Double) -> Weights n -> (Weights n,Vector Double)

class Default a where
  auto :: a

newtype NelderMeadsParams = NelderMeadsParams Int

instance Default NelderMeadsParams where
  auto = NelderMeadsParams 1000

data MinSettings = Annealing SimulatedAnnealingParams | NelderMeads NelderMeadsParams

instance Default SimulatedAnnealingParams where
  auto = SimulatedAnnealingParams 100 100 1.0 1.0 100 1.5 10

instance Default MinSettings where
  auto = NelderMeads auto

data PerEpisodeParams = PerEpisodeParams {
    _groupedBy :: Int,
    _iters :: Int
}
makeLenses ''PerEpisodeParams

instance Default PerEpisodeParams where
  auto = PerEpisodeParams 100 10

data NormalParams = NormalParams {
    _numMinimizations :: Int,
    _iterRange :: (Int,Int)
}
makeLenses ''NormalParams

instance Default NormalParams where
  auto = NormalParams 2 (300,600)

data MinimizationType = PerEpisode PerEpisodeParams | Normal NormalParams

instance Default MinimizationType where
  auto = Normal auto

data Settings = Settings {
    _typeMin :: MinimizationType,
    _settings :: MinSettings,
    _seed :: Int,
    _systems :: Int
}
makeLenses ''Settings

instance Default Settings where
  auto = Settings auto auto 230948 3

data NeuralSim system w n ins outs = NeuralSim {
    _minSettings       :: Settings,
    _simulatorInstance :: Simulator system,
    _startBox          :: BrainBox ins outs w n,
    _randTrainingState :: Double -> Weights w -> system,
    _neuralStep :: system -> Weights w -> system
}
makeLenses ''NeuralSim

minimizeS :: (KnownNat n) => NelderMeadsParams -> Minimizer n
minimizeS (NelderMeadsParams iterations) cost xi =
  minimizeV NMSimplex2 0.0001 iterations (V.replicate (ssize xi) 1) (fromVec &. cost) (toVec xi)
  & second (toColumns &. (!!1))
  & first fromVec

annealing :: (KnownNat n) => SimulatedAnnealingParams -> Minimizer n
annealing anParams cost xi = (simanSolve 123 (ssize xi) anParams xi cost metricDist stepFunction Nothing, V.empty)
  where metricDist a1 a2 = sZipWith (\x1 x2 -> (x1-x2)*(x1-x2)) a1 a2 & ssum & sqrt
        stepFunction rands stepSize current = rands & fromVec &> (\x -> x*2*stepSize - stepSize) & sZipWith (+) current

toMinimizer :: KnownNat n => MinSettings -> Minimizer n
toMinimizer (NelderMeads pars) = minimizeS pars
toMinimizer (Annealing pars) = annealing pars

minimizer :: (KnownNat n,KnownNat w) => NeuralSim system w n ins outs -> IO ()
minimizer neuralSim@(NeuralSim settings sim box randTrainingState neuralStep) =
  printRunner typeMin (toMinimizer minSettings) neuralSim
  where
    (Settings typeMin minSettings seed numSystems) = settings
    printRunner (PerEpisode _) = printMinimizer
    printRunner (Normal _)     = printAlternateMinimizer


networkMinimizer :: Minimizer n -> NeuralSim a n w ins outs -> ([Vector Double],Weights w, [(Double,Double)])
networkMinimizer optimizer (NeuralSim _ simulator startBox randTrainingState _) =
  (paths,weightRestorer minimizedWeights, zip (defaultCosts startWeights) (defaultCosts minimizedWeights))
    where
        (_, startWeights, weightRestorer) = startBox
        generalSeed = 2342344
        optiters = 1000
        simIterRange = (200,200)
        numSystemsPerMinimization = 10
        numMinimizations = 1
        defaultCosts weights = [100,200..1000] &> costOfRun (randTrainingState generalSeed weights)
        costOfRun state iters = simulateN simulator (iters::Int) state & fst
        randIter seed = pseudoRand simIterRange seed & floor
        sseeds = pseudoRands (0,1000) generalSeed & chunksOf numSystemsPerMinimization
                                                  & take numMinimizations
        (minimizedWeights,paths) = mapAccumL minimizeWeightSeeds startWeights sseeds

        minimizeWeightSeeds weights seeds =
            optimizer (costOfFlatPerSeeds seeds) weights

        costOfFlatPerSeeds seeds weights =
            seeds &> randIter
                  &> costOfRun (randTrainingState generalSeed weights)
                  &> (^^2)
                  & sum


perEpisodeMinimizer :: Minimizer n -> NeuralSim a n w ins outs -> Weights w
perEpisodeMinimizer optimizer (NeuralSim _ simulator (_, startWeights, weightRestorer) randTrainingState neuralStep) =
  (weightRestorer minimizedWeights)
    where
      generalSeed = 23423444
      optiters = 3000
      numSystemsPerMinimization = 100
      numIters = 7
      groupedBy = 70
      Simulator _ step cost defaultState = simulator
      randSystems = seeds &> flip randTrainingState startWeights
        where seeds = pseudoRands (0,1000) generalSeed & take numSystemsPerMinimization
      optimize (currSystems,weights,prevP) =
          optimizer (neuralStepsN currSystems &. costs) weights
          & (\(w,p) -> (neuralStepsN currSystems w,w,p:prevP))
        where costs systems = systems &!> (cost &. (^^2)) & sum & (^^2)
              neuralStepsN systems weights =
                apply groupedBy (flip neuralSteps weights) systems
              neuralSteps systems weights = systems &> (\s -> neuralStep s weights)
      (_,minimizedWeights,path) = apply numIters optimize (randSystems,startWeights,[])



printMinimizer :: _ => Minimizer n -> NeuralSim a n w ins outs -> IO ()
printMinimizer optimizer neuralSim = do
  mapM_ print a
  print b
  print c
    where (a,b,c) = networkMinimizer optimizer neuralSim

printAlternateMinimizer :: _ => Minimizer n -> NeuralSim a n w ins outs -> IO ()
printAlternateMinimizer optimizer neuralSim = do
  print (perEpisodeMinimizer optimizer neuralSim)
