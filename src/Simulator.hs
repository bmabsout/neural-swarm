{-# LANGUAGE TemplateHaskell #-}
module Simulator (module Simulator) where
import Graphics.Gloss
import Control.Lens
import Convenience
import Numeric.FastMath()

data Simulator a = Simulator {
    _simRender :: a -> Picture,
    _simStep :: a -> a,
    _simCost :: a -> Double,
}
makeLenses ''Simulator

joinSims :: [a] -> Simulator a -> Simulator [a]
joinSims s = Simulator {
    _simRender = \l -> l &> s^.simRender & pictures,
    _simStep = \l -> l &> s^.simStep,
    _simCost = \l -> l &> s^.simCost & sum
}

simulateN :: (Num a, Ord a) => Simulator t -> a -> t -> (Double, t)
simulateN (Simulator _ simStep simCost _) n initState =
    apply n (\(count,accost,state) ->
                 let newState = simStep state
                     newCost = accost + if count > 198 then (simCost newState)^2 else 0
                 in (if count > n then 0 else count + 1, newCost, newState))
            (0,0,initState)
     & (\(_,cost,finalState) -> (cost,finalState))


trackCost :: Int -> Simulator a -> [Double]
trackCost iterations (Simulator _ simStep simCost mainState) = iterate (apply iterations simStep) mainState &> simCost


