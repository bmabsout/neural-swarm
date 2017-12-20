
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}

module Minimizer(module Minimizer) where

import           Brain
import           Control.Arrow
import           Control.Lens
import           Control.Monad.Random
import           Convenience
import qualified Data.Vector.Storable           as V
import           GHC.TypeLits
import           Numeric.GSL.Minimization
import           Numeric.GSL.SimulatedAnnealing
import           Simulator
import qualified Graphics.Rendering.Chart.Easy  as E
import qualified Graphics.Rendering.Chart.Gtk   as C
import           Numeric.LinearAlgebra.Data
import           System.IO.Unsafe
import qualified Numeric.AD as AD

type Minimizer n = (forall a. Floating a => Weights n a -> a) -> Weights n Double -> Weights n Double


newtype NelderMeadsParams = NelderMeadsParams Int

instance Default NelderMeadsParams where
  auto = NelderMeadsParams 1000


instance Default SimulatedAnnealingParams where
  auto = SimulatedAnnealingParams 100 100 1.0 1.0 100 1.5 10


newtype AutoDiffParams = AutoDiffParams Int

instance Default AutoDiffParams where
  auto = AutoDiffParams 10


data MinSettings = Annealing SimulatedAnnealingParams | NelderMeads NelderMeadsParams | AutoDiff AutoDiffParams


instance Default MinSettings where
  auto = AutoDiff auto


data Settings = Settings {
    _minSettings      :: MinSettings,
    _seed             :: Int,
    _systems          :: Int,
    _groupedBy        :: Int,
    _iterRange        :: (Int,Int),
    _numMinimizations :: Int
}
makeLenses ''Settings

instance Default Settings where
  auto = Settings {
    _minSettings = auto,
    _seed = 4374653543,
    _systems = 3,
    _groupedBy = 30,
    _iterRange = (400,400),
    _numMinimizations = 5
  }

data NeuralSim system enabled all f = NeuralSim {
    _settings   :: Settings,
    _weights    :: Weights enabled f,
    _restorer   :: Restorer enabled all f,
    _setWeights :: forall a . Floating a => Weights enabled a -> system a -> system a
}
makeLenses ''NeuralSim

minimizeS :: (KnownNat n) => NelderMeadsParams -> Minimizer n
minimizeS (NelderMeadsParams iterations) cost xi =
  minimizeV NMSimplex2 0.0001 iterations (V.replicate (ssize xi) 0.2) (fromVec &. cost) (toVec xi)
  -- & chartify
  & fst & fromVec

chartify :: (a, Matrix Double) -> (a, ())
chartify = second (toColumns &. (!!1)
            &. Numeric.LinearAlgebra.Data.toList
            &. zip [1::Int ..]
            &. (:[]) &. E.line "30" &. E.plot
            &. C.toWindow 500 500 &. unsafePerformIO)
            &. (\t -> seq (snd t) t)

annealing :: (KnownNat n) => SimulatedAnnealingParams -> Minimizer n
annealing anParams cost xi = simanSolve 123 (ssize xi) anParams xi cost metricDist stepFunction (Just $ const "")
  where metricDist a1 a2 = sZipWith (\x1 x2 -> (x1-x2)*(x1-x2)) a1 a2 & ssum & sqrt
        stepFunction rands stepSize current = rands & fromVec &> (\x -> x*2*stepSize - stepSize) & sZipWith (+) current


automaticDiff :: (KnownNat n) => AutoDiffParams -> Minimizer n
automaticDiff (AutoDiffParams iterations) cost (Sized weights) = AD.conjugateGradientDescent (Sized &. cost) weights & take iterations & last & Sized

toMinimizer :: KnownNat n => MinSettings -> Minimizer n
toMinimizer (NelderMeads pars) = minimizeS pars
toMinimizer (Annealing pars)   = annealing pars
toMinimizer (AutoDiff pars)    = automaticDiff pars

minimizer :: (KnownNat all,KnownNat enabled,Simulator system) => NeuralSim system enabled all Double -> IO ()
minimizer n = print (evalRand (minimizeRand n) (mkStdGen $ n^.settings.seed))


minimizeRand :: (KnownNat all, KnownNat enabled, Simulator system) =>
                    NeuralSim system enabled all Double -> Rand StdGen ([(Double,Double)], Weights all Double)
minimizeRand n = composeN (n^.settings.numMinimizations) episodic ([], n^.weights)
                 &> second (n^.restorer)
  where
    episodic (costs,weights) =
      episodicM (toMinimizer (n^.settings.minSettings)) (n^.settings) (n^.setWeights) weights
      &> first (:costs)


episodicM :: forall s enabled. (Simulator s)
          => Minimizer enabled
          -> Settings 
          -> (forall a . (Floating a) => Weights enabled a -> s a -> s a)
          -> Weights enabled Double
          -> Rand StdGen ((Double,Double), Weights enabled Double)
episodicM optimizer settins weightSetter initialWeights = do
  iters <- getRandomR (settins^.iterRange)
  randSystems <- getRandoms &> take (settins^.systems)

  let episodeSize = settins^.groupedBy

  let simsEpisode :: Floating a => [s a] -> [s a]
      simsEpisode = apply episodeSize simsStep

  let episodes = iters `div` episodeSize

  let episodedSystems :: Floating a => [[s a]]
      episodedSystems = randSystems
                        &> realToFracSim
                        &  iterate simsEpisode
                        &  take episodes

  let costOfWeights :: (Floating a) => Weights enabled a -> a
      costOfWeights ws = episodedSystems
                         &>>weightSetter ws
                         &> simsEpisode
                         &> simsCost
                         &  sum

  let resultWeights = optimizer costOfWeights initialWeights
  return ((costOfWeights initialWeights,costOfWeights resultWeights), resultWeights)


-- inputRecreator :: _ => Brain n n numWeights -> Minimizer numWeights -> g (Weights numWeights)
-- inputRecreator (Brain feed) optimizer =
--   getRandoms &>> randWeights
--              &> (\inputs -> optimizer (\w -> let outputs = inputs &> feed w
--                                              in zipWith hamming inputs outputs & sum)
--                                       (randWeights 23423))
--   where
--     hamming :: Weights n -> Weights n -> Double
--     hamming w1 w2 = sZipWith (\x y -> (x-y)**2) w1 w2 & sum
