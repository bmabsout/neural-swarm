
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
import           Control.Monad.Random

type Minimizer n = (Weights n -> Double) -> Weights n -> (Weights n,Vector Double)

newtype NelderMeadsParams = NelderMeadsParams Int

instance Default NelderMeadsParams where
  auto = NelderMeadsParams 1000

data MinSettings = Annealing SimulatedAnnealingParams | NelderMeads NelderMeadsParams

instance Default SimulatedAnnealingParams where
  auto = SimulatedAnnealingParams 100 100 1.0 1.0 100 1.5 10

instance Default MinSettings where
  auto = NelderMeads auto


data Settings = Settings {
    _minSettings :: MinSettings,
    _seed :: Int,
    _systems :: Int,
    _groupedBy :: Int,
    _iterRange :: (Int,Int),
    _numMinimizations :: Int
}
makeLenses ''Settings

instance Default Settings where
  auto = Settings {
    _minSettings = auto,
    _seed = 234234,
    _systems = 3,
    _groupedBy = 50,
    _iterRange = (300,500),
    _numMinimizations = 2
  }

data NeuralSim system enabled all = NeuralSim {
    _settings          :: Settings,
    _simulatorInstance :: Simulator system,
    _weights           :: Weights enabled,
    _restorer          :: Restorer enabled all,
    _randTrainingState :: forall g. Weights enabled -> Rand g system,
    _neuralStep :: system -> Weights enabled -> system
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

minimizer :: (KnownNat all,KnownNat enabled) => NeuralSim system enabled all -> IO ()
minimizer neuralSim@(NeuralSim settings sim _ _ randTrainingState neuralStep) =
  printMinimizer (toMinimizer (settings^.minSettings)) neuralSim


merged :: Minimizer n -> Settings -> Simulator system -> Weights w
merged optimizer settings simulator = undefined


-- networkMinimizer :: Minimizer n -> NeuralSim a n w -> ([Vector Double],Weights w, [(Double,Double)])
-- networkMinimizer optimizer (NeuralSim _ simulator startWeights weightRestorer randTrainingState _) =
--   (paths,weightRestorer minimizedWeights, zip (defaultCosts startWeights) (defaultCosts minimizedWeights))
--     where
--         generalSeed = 2342344
--         optiters = 1000
--         simIterRange = (200,200)
--         numSystemsPerMinimization = 10
--         numMinimizations = 1
--         defaultCosts weights = [100,200..1000] &> costOfRun (randTrainingState generalSeed weights)
--         costOfRun state iters = simulateN simulator (iters::Int) state & fst
--         randIter seed = pseudoRand simIterRange seed & floor
--         sseeds = pseudoRands (0,1000) generalSeed & chunksOf numSystemsPerMinimization
--                                                   & take numMinimizations
--         (minimizedWeights,paths) = mapAccumL minimizeWeightSeeds startWeights sseeds

--         minimizeWeightSeeds weights seeds =
--             optimizer (costOfFlatPerSeeds seeds) weights

--         costOfFlatPerSeeds seeds weights =
--             seeds &> randIter
--                   &> costOfRun (randTrainingState generalSeed weights)
--                   &> (^^2)
--                   & sum


-- perEpisodeMinimizer :: Minimizer n -> NeuralSim a n w -> Weights w
-- perEpisodeMinimizer optimizer (NeuralSim _ simulator startWeights weightRestorer randTrainingState neuralStep) =
--   (weightRestorer minimizedWeights)
--     where
--       generalSeed = 23423444
--       optiters = 3000
--       numSystemsPerMinimization = 100
--       numIters = 7
--       groupedBy = 70
--       Simulator _ step cost defaultState = simulator
--       randSystems = seeds &> flip randTrainingState startWeights
--         where seeds = pseudoRands (0,1000) generalSeed & take numSystemsPerMinimization
--       optimize (currSystems,weights,prevP) =
--           optimizer (neuralStepsN currSystems &. costs) weights
--           & (\(w,p) -> (neuralStepsN currSystems w,w,p:prevP))
--         where costs systems = systems &!> (cost &. (^^2)) & sum & (^^2)
--               neuralStepsN systems weights =
--                 apply groupedBy (flip neuralSteps weights) systems
--               neuralSteps systems weights = systems &> (\s -> neuralStep s weights)
--       (_,minimizedWeights,path) = apply numIters optimize (randSystems,startWeights,[])



-- printMinimizer :: _ => Minimizer n -> NeuralSim a n w -> IO ()
-- printMinimizer optimizer neuralSim = do
--   mapM_ print a
--   print b
--   print c
--     where (a,b,c) = networkMinimizer optimizer neuralSim

-- printAlternateMinimizer :: _ => Minimizer n -> NeuralSim a n w -> IO ()
-- printAlternateMinimizer optimizer neuralSim = do
--   print (perEpisodeMinimizer optimizer neuralSim)
