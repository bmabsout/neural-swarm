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

neuralChart :: forall (m :: Nat) a (t :: Nat) (t1 :: Nat) (t2 :: Nat) b (n1 :: Nat).
                       (KnownNat m, RealFloat a, Enum a) =>
                       Proxy (m + 1)
                       -> Int
                       -> NeuralSim b a ((m + n1) + 1) t t1 t2 -> [(a, a)]

neuralChart weightIndex numIters (NeuralSim simulator (_, startWeights, _) randTrainingState neuralStep) =
  zip weightVals (costsOf weightVals weightIndex)
    where
      weightVals = [-40,-39.9 .. 40]
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

singleWeightChangeChart :: _ => NeuralSim a b _ d e f -> IO ()
singleWeightChangeChart neuralSim = toWindow 500 500 $ do
    layout_title .= "Amplitude Modulation"
    setColors [opaque blue, opaque red, opaque green, opaque purple]
    plot (line "am" ([neuralChart (Proxy @10) 10 neuralSim]))
    plot (line "pm" ([neuralChart (Proxy @10) 100 neuralSim]))
    plot (line "cm" ([neuralChart (Proxy @10) 250 neuralSim]))
