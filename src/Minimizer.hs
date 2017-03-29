
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

type Minimizer n = (Weights n -> Double) -> Weights n -> Weights n

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
    _seed = 23423334,
    _systems = 3,
    _groupedBy = 30,
    _iterRange = (300,500),
    _numMinimizations = 2
  }

data NeuralSim system enabled all = NeuralSim {
    _settings          :: Settings,
    _weights           :: Weights enabled,
    _restorer          :: Restorer enabled all,
    _setWeights        :: Weights enabled -> system -> system
}
makeLenses ''NeuralSim

minimizeS :: (KnownNat n) => NelderMeadsParams -> Minimizer n
minimizeS (NelderMeadsParams iterations) cost xi =
  minimizeV NMSimplex2 0.0001 iterations (V.replicate (ssize xi) 1) (fromVec &. cost) (toVec xi)
  & fst & fromVec
  -- & second (toColumns &. (!!1))

annealing :: (KnownNat n) => SimulatedAnnealingParams -> Minimizer n
annealing anParams cost xi = simanSolve 123 (ssize xi) anParams xi cost metricDist stepFunction Nothing
  where metricDist a1 a2 = sZipWith (\x1 x2 -> (x1-x2)*(x1-x2)) a1 a2 & ssum & sqrt
        stepFunction rands stepSize current = rands & fromVec &> (\x -> x*2*stepSize - stepSize) & sZipWith (+) current

toMinimizer :: KnownNat n => MinSettings -> Minimizer n
toMinimizer (NelderMeads pars) = minimizeS pars
toMinimizer (Annealing pars) = annealing pars

minimizer :: (KnownNat all,KnownNat enabled,Simulator system) => NeuralSim system enabled all -> IO ()
minimizer n = print (evalRand (minimizeRand n) (mkStdGen $ n^.settings.seed))


minimizeRand :: (KnownNat all, KnownNat enabled, CostStep system, Random system) =>
                    NeuralSim system enabled all -> Rand StdGen (Weights enabled)
minimizeRand n = composeN (n^.settings.numMinimizations) episodic (n^.weights)
  where
    episodic = episodicM (toMinimizer (n^.settings.minSettings)) (n^.settings) (n^.setWeights)

episodicM :: (CostStep s,Random s) => Minimizer enabled -> Settings -> (Weights enabled -> s -> s) -> Weights enabled -> Rand StdGen (Weights enabled)
episodicM optimizer settins weightSetter initialWeights = do
  iters <- getRandomR (settins^.iterRange)
  systems <- getRandoms &> take (settins^.systems)
  let episodic = apply episodes perEpisode (systems, initialWeights) & snd
        where episodeSize = settins^.groupedBy
              episodes = iters `div` episodeSize
              perEpisode (ss,ws) = (simEpisode ss, optimizer (\w -> simCost $ simEpisode (ss &> weightSetter w)) ws)
                where simEpisode = apply episodeSize simStep
  return episodic
