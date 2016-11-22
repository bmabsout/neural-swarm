{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Runner(runner) where

import FireFlies
import Test
import Boids
import Simulator
import Minimizer

import Graphics.Gloss

currentInstance :: Simulator (Flies _ Int) Float
currentInstance = fliesSimulatorInstance

neuralInstance :: NeuralSim (Flies _ Int) Double _
neuralInstance = fliesNeuralSimInstance

stage :: Display
stage = InWindow "Simulation" (200,200) (10,10)

fps = 60

runner :: Int -> IO ()
runner 0 = printMinimizer neuralInstance
runner 1 = let Simulator simRender simStep _ mainState = currentInstance
           in simulate stage black (floor fps) mainState simRender (\_ _ a -> simStep a)
runner 2 = mapM_ print (trackCost currentInstance)
