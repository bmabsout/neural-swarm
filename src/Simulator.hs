
module Simulator (module Simulator) where
import Graphics.Gloss
import Control.Lens
import Control.Arrow
import Convenience
import Numeric.FastMath()
import Control.Monad.Random


class CanRender a where
    simRender :: a Double -> Picture

class Steppable a where
    simStep :: Floating f => a f -> a f

class HasCost a where
    simCost :: Floating float => a float -> float

type CostStep a = (HasCost a, Steppable a)

class (CostStep a, CanRender a,Random (a Double)) => Simulator a where
    realToFracSim :: Floating floating => a Double -> a floating

simulateN :: (CostStep t,Floating float) => Int -> t float -> (float, t float)
simulateN n initState = apply n simStep initState & simCost &&& id


trackCost :: (Default (a Double), CostStep a) => Int -> a Double -> [Double]
trackCost iterations a = iterate (apply iterations simStep) a &> simCost

simsRender l = l &!> simRender & pictures
simsStep l   = l &!> simStep
simsCost l   = l &!> simCost & sum
