{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module FireFlies
    ( Flies,
      fliesSimulatorInstance,
      fliesNeuralSimInstance
    ) where

import           Control.Lens
import           Graphics.Gloss
import           Data.List
import qualified Data.Vector.Storable as V
import           Numeric.FastMath()
import           Convenience
import           Brain
import           Simulator
import           Minimizer

timepf = 1/60

type Syncer a = [a] -> a
type Fly a = (Vec a, a)
data Flies a = Flies {
    _period           :: a,
    _flyNeighbhorhood :: Int,
    _times            :: [a],
    _positions        :: [Vec a],
    _size             :: Int,
    _matrix           :: [[Int]],
    _syncer           :: Syncer a}
makeLenses ''Flies


nonSyncer = randFlies (10,20) 234 5 (\_ _ -> 0) False

fliesSimulatorInstance :: (Ord a,RealFloat a,V.Storable a) => Simulator (Flies a) a
fliesSimulatorInstance = Simulator simRender simStep simCost mainState
  where
    simCost (Flies p n ts _ s m _) = m &> newGetIxs (V.fromList ts)
                                       &. allWithAll p (fromIntegral n)
                                       & sum & (/ fromIntegral s) & realToFrac
        where allWithAll p s l = l &> (\a -> l &> (\b ->timeDist p a b ^^2) & sum)
                                   & sum & (/ s^^2)

    simRender fs = zipWith (renderFirefly (fs^.period)) (fs^.positions) (fs^.times)
                   & pictures
        where
            renderFirefly :: RealFloat a => a -> Vec a -> a -> Picture
            renderFirefly period pos time = circleSolid 8 & vecTranslate pos
                                                          & color currentColor
                where vecTranslate (Vec (x,y)) = translate (realToFrac x) (realToFrac y)
                      currentColor = let t = realToFrac (time / period)
                                     in makeColor t t t 1


    simStep fs@(Flies per _ ts _ _ matrix syncer) = fs & times .~ newTimes
        where
          newTimes =
               ts &> (+ realToFrac timepf)
                  & zipWith (\indices currT ->
                                 if currT >= per
                                 then currT - per + sync indices (V.fromList ts) syncer
                                 else currT)
                            matrix
            where sync indices ts syncer = indices & newGetIxs ts & syncer

    mainState = randFlies (100,100) 203430 5 (applyBeforeBox rnnBox neuralSyncer) False


type NumNeighbhors = 5

fliesNeuralSimInstance :: (RealFloat a,Ord a,V.Storable a) =>
                              NeuralSim (Flies a) a _ _
fliesNeuralSimInstance = NeuralSim fliesSimulatorInstance currentBox randTrainingState
  where
    currentBox@(brain,_,_) = rnnBox
    numNeighbhors = typeNum (Proxy :: Proxy NumNeighbhors)

    randTrainingState seed weights =
        randFlies (10,200) (seed+1) numNeighbhors (neuralSyncer brain weights) False


newGetIxs vec indices = indices & V.fromList & V.map fromIntegral
                                & V.backpermute vec & V.toList


neuralSyncer :: _ => Brain a _ _ w -> Weights w a -> a -> Syncer a
neuralSyncer (Brain feedForward) weights period inputs = feedForward weights (fromList inputs) & shead & (*(-period))

mySyncer :: (Ord a,Fractional a) => a -> Syncer a
mySyncer period l = l &> ringDiff period & mean
  where ringDiff n t = t + if t > n/2 then -n+t else t
        mean [] = 0
        mean l  = sum l / fromIntegral (length l)


randFlies :: RealFloat a => (a,a) -> a -> Int -> (a -> Syncer a) -> Bool -> Flies a
randFlies numFliesRange seed numNeighbhors syncer singleLine =
    Flies per numNeighbhors tgen vecs size (closestIs numNeighbhors vecs) (syncer per)
    where
      per = 0.5
      size = floor numFlies
      numFlies = pseudoRand numFliesRange (seed+3)
      xgen | singleLine = iterate (+16) (-numFlies*8)
           | otherwise  = pseudoRands (-500,500) (seed+2)

      ygen | singleLine = repeat 0
           | otherwise  = pseudoRands (-500,500) (seed+1)
      vecs = zip xgen ygen & take size &> Vec
      tgen = pseudoRands (0,per) seed & take size

-- closests :: (Ord a,Num a) =>  Flies a -> Fly a -> Flies a
-- closests (Flies t pos _) (fp,_) = (zip pos t) & sortBy (comparing (\(p,_) -> magVsq (fp-p))) & tail

closestIs :: (Fractional a,Ord a) => Int -> [Vec a] -> [[Int]]
closestIs n l = l &> sortDist withIs
    where withIs = zip [0..] l
          sortDist l e = l & sortOn (snd &. distsq e)
                           & tail & take n &> fst


timeDist :: (Ord a, Num a) => a -> a -> a -> a
timeDist n t1 t2 = min absDiff (n - absDiff)
  where absDiff = abs (t1 - t2)


rnnBox :: _ => BrainBox t NumNeighbhors 1 _ _
rnnBox = buildBrain (initBrain (joinSized rnnWeights (randWeights 2343)) #> (recurrent Proxy (biased @3 @3 \> biased @2)) #> biased @3 #> biased)

rnnWeights :: (RealFloat a, V.Storable a) => Weights 33 a
rnnWeights = mkN -1.4745158501997837 1.0474981651993653 2.514310082942333 -6.700791116978616 1.8186054096005706 0.46455878607044787 -3.283423897636994 -2.0138468641953304 -1.7085708702833413 1.4137516503722205 3.3944371094042767 1.3962744033656946 -1.4835509690540132 1.5785628013607749 0.593040653176141 -1.1611137430268412 -0.7426096110351696 4.003758057624214 -2.100649905842519 1.9537581051549484 0.3164098989655858 1.5459213100034765 -1.6027827805687944 1.9876418672351264 1.5492973981156304 1.3623563284519289 1.6157329890403433 1.3585711448287672 -2.2217496953565075 -2.2970150445993642 2.296035319553119 -1.1683391142143993 2.823441346524306




reallySmallBox :: _ => BrainBox t NumNeighbhors 1 _ _
reallySmallBox = buildBrain (initBrain reallysmallWeights #> Disable (biased @3) #> biased @3 #> biased)

reallysmallWeights :: (RealFloat a, V.Storable a) => Weights 34 a
reallysmallWeights = mkN -0.7334381641872847 1.0938472660172938 1.0660863550775022 2.4185786519725396 2.3936844141880522 2.1486174159519726 -1.001344026934531 -0.865626178993556 -0.3754357835138557 0.26613213327252705 0.28793641294238603 0.49556430626227277 -1.03561251581661 -0.45290830123844117 -0.38028426554067485 -0.6010359243134789 -0.2677604523204588 9.926473410559172e-2 -1.5796644674274913 3.3350435131062746 -0.16275227168160122 -5.4740616916753436e-2 0.5903334518757735 -1.4122912769204028 0.607711956873247 0.6154003132545869 -0.7563804638605576 -0.7180158302857877 -1.5843572795322811 -0.7669881269471803 -1.0720646428106975 1.8170116882560636 -1.7281382427769225 -0.7752604712197634
