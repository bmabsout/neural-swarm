{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Runner(runner) where

import FireFlies
import Simulator
import Boids
import Test
import Minimizer

import Graphics.Gloss

currentInstance :: Simulator _ Double
currentInstance = boidsSimulatorInstance

neuralInstance = boidsNeuralInstance

stage :: Display
stage = InWindow "Simulation" (200,200) (10,10)

fps = 60

runner :: Int -> IO ()
runner 0 = printMinimizer neuralInstance
runner 1 = let Simulator simRender simStep _ mainState = currentInstance
           in simulate stage black fps mainState simRender (\_ _ a -> simStep a)
runner 2 = mapM_ print (trackCost 100 currentInstance)

