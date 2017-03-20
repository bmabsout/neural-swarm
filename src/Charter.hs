{-# LANGUAGE FlexibleContexts #-}
module Charter(singleWeightChangeChart) where

import GHC.TypeLits
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Gtk
import Minimizer
import Control.Lens
import Data.Proxy
import Brain
import Simulator
import Convenience
import SizedL

rangedReplace :: _ => [a] -> Sized _ a -> Proxy _ -> [Sized _ a]
rangedReplace rep xs index = rep &> replaceAt index xs

neuralChart :: forall (m :: Nat) (t :: Nat) b (n1 :: Nat).
                       (KnownNat m) =>
                       Proxy (m + 1)
                       -> Int
                       -> NeuralSim b ((m + n1) + 1) t -> [(Double, Double)]

neuralChart weightIndex numIters (NeuralSim _ simulator startWeights _ randTrainingState neuralStep) =
  zip weightVals (costsOf weightVals weightIndex)
    where
      weightVals = [-40,-39 .. 40]
      generalSeed = 2342344
      numSystemsPerMinimization = 10
      Simulator _ step cost defaultState = simulator
      randSystems = seeds &> flip randTrainingState startWeights
        where seeds = pseudoRands (0,1000) generalSeed & take numSystemsPerMinimization
      costsOf xs index = rangedReplace xs startWeights index &> neuralStepsN randSystems &> costs
        where costs systems = systems &> cost &> (^^2) & sum & (^^2)
              neuralStepsN systems weights =
                apply numIters (neuralSteps weights) systems
              neuralSteps weights systems = systems &> (\s -> neuralStep s weights)

signal :: [Double] -> [(Double,Double)]
signal = map $ \x -> (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5)))

singleWeightChangeChart :: _ => NeuralSim a _ d -> IO ()
singleWeightChangeChart neuralSim = toWindow 500 500 $ do
    layout_title .= "Amplitude Modulation"
    setColors [opaque blue, opaque red, opaque green, opaque purple]
    -- plot (line "10" ([neuralChart (Proxy @1) 10 neuralSim]))
    -- plot (line "20" ([neuralChart (Proxy @1) 20 neuralSim]))
    plot (line "30" ([neuralChart (Proxy @1) 100 neuralSim]))
    -- plot (line "50" ([neuralChart (Proxy @2) 100 neuralSim]))
