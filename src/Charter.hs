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
import Control.Monad.Random

rangedReplace :: _ => [a] -> Sized _ a -> Proxy _ -> [Sized _ a]
rangedReplace rep xs index = rep &> replaceAt index xs

neuralChart :: forall (m :: Nat) (t :: Nat) b (n1 :: Nat).
                       (KnownNat m, Simulator b) =>
                       Proxy (m + 1)
                       -> Int
                       -> NeuralSim b ((m + n1) + 1) t Double -> [(Double, Double)]
neuralChart weightIndex numIters (NeuralSim _ startWeights _ nSet) = undefined

singleWeightChangeChart :: _ => NeuralSim a _ d Double -> IO ()
singleWeightChangeChart neuralSim = toWindow 500 500 $ do
    layout_title .= "Amplitude Modulation"
    setColors [opaque blue, opaque red, opaque green, opaque purple]
    -- plot (line "10" ([neuralChart (Proxy @1) 10 neuralSim]))
    -- plot (line "20" ([neuralChart (Proxy @1) 20 neuralSim]))
    plot (line "30" ([neuralChart (Proxy @1) 100 neuralSim]))
    -- plot (line "50" ([neuralChart (Proxy @2) 100 neuralSim]))
