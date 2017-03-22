{-# LANGUAGE TemplateHaskell #-}
module Simulator (module Simulator) where
import Graphics.Gloss
import Control.Lens
import Convenience
import Numeric.FastMath()


class CanRender a where
    simRender :: a -> Picture

class Steppable a where
    simStep :: a -> a

class HasCost a where
    simCost :: a -> Double

type CostStep a = (HasCost a, Steppable a)

type Simulator a = (CostStep a, CanRender a)

simulateN :: (Num a, Ord a, CostStep t) => a -> t -> (Double, t)
simulateN n initState =
    apply n (\(count,accost,state) ->
                 let newState = simStep state
                     newCost = accost + if count > 198 then (simCost newState)^2 else 0
                 in (if count > n then 0 else count + 1, newCost, newState))
            (0,0,initState)
     & (\(_,cost,finalState) -> (cost,finalState))


trackCost :: (Default a, CostStep a) => Int -> a -> [Double]
trackCost iterations a = iterate (apply iterations simStep) a &> simCost


