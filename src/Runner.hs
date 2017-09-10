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

-- currentInstance = auto @Flies
-- neuralInstance = fliesNeuralInstance
-- currentInstance = auto @Test
-- neuralInstance = testNeuralInstance
currentInstance = auto @(Boids 4)
neuralInstance = boidsNeuralInstance
-- currentInstance = auto @Filler
-- neuralInstance = fillerNeuralInstance


stage :: Display
stage = InWindow "Simulation" (200,200) (10,10)

fps = 60

(|=) = singleton

actions :: Map [String] (IO ())
actions = mconcat [
    ["train"]     |=  minimizer neuralInstance,
    ["run"]       |=  simulate stage black fps currentInstance simRender (\_ _ a -> simStep a),
    ["test"]      |=  mapM_ print (trackCost 100 currentInstance),
    ["chart"]     |=  singleWeightChangeChart neuralInstance]

runner :: [String] -> IO ()
runner args = findWithDefault (putStrLn $ "wrong arguments choose : " ++ helpString) args actions
    where helpString = keys actions &> unwords & intersperse " | " & mconcat


