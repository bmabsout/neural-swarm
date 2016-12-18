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

    mainState = randFlies (10,10) 203430 5 (applyBeforeBox reallySmallBox neuralSyncer) True


type NumNeighbhors = 5

fliesNeuralSimInstance :: (RealFloat a,Ord a,V.Storable a) =>
                              NeuralSim (Flies a) a _ _
fliesNeuralSimInstance = NeuralSim fliesSimulatorInstance currentBox randTrainingState neuralStep
  where
    currentBox@(brain,_,_) = reallySmallBox
    numNeighbhors = typeNum (Proxy :: Proxy NumNeighbhors)

    neuralStep system weights =
      _simStep fliesSimulatorInstance (system & syncer .~ (neuralSyncer brain weights (system^.period)))
    randTrainingState seed weights =
        randFlies (10,10) (seed+1) numNeighbhors (neuralSyncer brain weights) True


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
rnnBox = buildBrain (initBrain rnnWeights #> (recurrent Proxy (biased @3 @3 \> biased @2)) #> biased @3 #> biased)

rnnWeights :: (RealFloat a, V.Storable a) => Weights 33 a
rnnWeights = mkN 0.5733087931295338 0.8843116035583927 0.9830967542071569 8.069029547587606e-2 0.8494013052245915 0.5147435987459859 0.31671157542727635 0.2929332074380583 0.11594838674512303 -0.21799856915503135 0.7506602746298818 -1.2018478917172448 -4.7336712761004804e-2 0.951387058910403 0.30171590558247674 0.4978051952463163 -0.5000584294988752 0.14617815919033744 -0.36632085833377115 1.4354171506330022 -0.21495207420824136 -0.7011369451564735 1.0350156769651155 0.7781496741742064 0.17379993858229503 0.7229360946979062 0.3135918408278803 0.44759171529078823 -0.858169598985294 0.637177720473048 -0.5570147426335263 -1.3689263428559317 0.9636319146515153



reallySmallBox :: _ => BrainBox t NumNeighbhors 1 _ _
reallySmallBox = buildBrain (initBrain reallysmallWeights #> (biased @3) #> biased @3 #> biased)

reallysmallWeights :: (RealFloat a, V.Storable a) => Weights 34 a
reallysmallWeights =mkN -3.485997443664722 3.451639097464124 4.6567863555356706 4.470679516204539 3.9304224519718254 4.742358370965663 0.7278736549802336 2.4365247834777284 4.700972937949945 1.2255097792956406 1.5108881599242179 2.015614765613079 -0.9432506251405637 -0.5775964416410124 -1.29058891032523 2.7151107459603425 0.3510448791637692 -1.196157361476466 -9.124757650115334e-2 -5.4653920470406465 -0.298434359392983 5.596502199172447 3.0185076881971398 -7.785398322055368 0.9611303021238209 3.0860859835503316 -0.15065477628538185 -1.7844407315762827 7.856067324479121e-2 2.069374025550582 -2.633304452556725 0.3048943975446088 -5.718433693248468 -0.13907868626636355
