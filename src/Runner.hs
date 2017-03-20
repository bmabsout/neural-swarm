module Runner(runner) where

import FireFlies
import Simulator
import Boids
import Test
import Minimizer
import Charter
import Convenience

import Data.Map
import Data.List
import Graphics.Gloss

currentInstance :: Simulator _
currentInstance = boidsSimulatorInstance

neuralInstance = boidsNeuralInstance

stage :: Display
stage = InWindow "Simulation" (200,200) (10,10)

fps = 60

(|=) = singleton

actions :: Map [String] (IO ())
actions = mconcat [
    ["train"]     |=  minimizer neuralInstance,
    ["run"]       |=  let Simulator simRender simStep _ mainState = currentInstance
                      in simulate stage black fps mainState simRender (\_ _ a -> simStep a),
    ["test"]      |=  mapM_ print (trackCost 100 currentInstance),
    ["chart"]     |=  singleWeightChangeChart neuralInstance]

runner :: [String] -> IO ()
runner args = findWithDefault (putStrLn $ "wrong arguments choose : " ++ helpString) args actions
    where helpString = keys actions &> unwords & intersperse " | " & mconcat


