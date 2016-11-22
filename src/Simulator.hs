module Simulator (module Simulator) where
import Graphics.Gloss
import Convenience
import Numeric.FastMath()

data Simulator a b = Simulator {
    _simRender :: a -> Picture,
    _simStep :: a -> a,
    _simCost :: a -> b,
    _mainState :: a
}

simulateN :: (Num a, Num t, Ord a) => Simulator t1 t -> a -> t1 -> (t, t1)
simulateN (Simulator _ simStep simCost _) n initState =
    apply n (\(count,accost,state) ->
                 let newState = simStep state
                     newCost = accost + if count > 10 then simCost newState else 0
                 in (if count > n then 0 else count + 1, newCost, newState))
            (0,0,initState)
     & (\(_,cost,finalState) -> (cost,finalState))


trackCost :: Simulator a b -> [b]
trackCost (Simulator _ simStep simCost mainState) = iterate (apply (1000::Int) simStep) mainState &> simCost


