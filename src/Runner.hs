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
currentInstance = fliesSimulatorInstance

neuralInstance = fliesNeuralSimInstance

stage :: Display
stage = InWindow "Simulation" (200,200) (10,10)

fps = 60

runner :: String -> IO ()
runner "train" = printMinimizer neuralInstance
runner "alternate" = printAlternateMinimizer neuralInstance
runner "run"   = let Simulator simRender simStep _ mainState = currentInstance
                 in simulate stage black fps mainState simRender (\_ _ a -> simStep a)
runner "test"  = mapM_ print (trackCost 100 currentInstance)
runner _       = print "wrong arg, try train run or test"

