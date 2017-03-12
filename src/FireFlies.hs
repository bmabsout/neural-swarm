{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NegativeLiterals #-}
module FireFlies
    ( Flies,
      fliesSimulatorInstance,
      fliesNeuralInstance
    ) where

import           Control.Lens
import           Graphics.Gloss
import           Data.List
import qualified Data.Vector.Storable as V
import           Numeric.FastMath()
import           Convenience
import           Data.Proxy
import           Brain
import           Simulator
import           Minimizer

timepf = 1/60

type Syncer = [Double] -> Double
type Fly = (Vec Double, Double)
data Flies = Flies {
    _period           :: Double,
    _flyNeighbhorhood :: Int,
    _times            :: [Double],
    _positions        :: [Vec Double],
    _size             :: Int,
    _matrix           :: [[Int]],
    _syncer           :: Syncer}
makeLenses ''Flies


nonSyncer = randFlies (10,20) 234 5 (\_ _ -> 0) False

fliesSimulatorInstance :: Simulator Flies
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

    mainState = randFlies (100,100) 203430 5 (applyBeforeBox reallySmallBox neuralSyncer) True


type NumNeighbhors = 5

fliesNeuralInstance :: NeuralSim Flies _ _ _ _
fliesNeuralInstance = NeuralSim auto fliesSimulatorInstance currentBox randTrainingState neuralStep
  where
    currentBox@(brain,_,_) = reallySmallBox
    numNeighbhors = typeNum (Proxy :: Proxy NumNeighbhors)

    neuralStep system weights =
      _simStep fliesSimulatorInstance (system & syncer .~ (neuralSyncer brain weights (system^.period)))
    randTrainingState seed weights =
        randFlies (100,100) (seed+1) numNeighbhors (neuralSyncer brain weights) True


newGetIxs vec indices = indices & V.fromList & V.map fromIntegral
                                & V.backpermute vec & V.toList


neuralSyncer :: _ => Brain _ _ w -> Weights w -> Double -> Syncer
neuralSyncer (Brain feedForward) weights period inputs = feedForward weights (fromList inputs) & shead & (*(-period))

mySyncer :: Double -> Syncer
mySyncer period l = l &> ringDiff period & mean
  where ringDiff n t = t + if t > n/2 then -n+t else t
        mean [] = 0
        mean l  = sum l / fromIntegral (length l)


randFlies :: (Double,Double) -> Double -> Int -> (Double -> Syncer) -> Bool -> Flies
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

closestIs :: Int -> [Vec Double] -> [[Int]]
closestIs n l = l &> sortDist withIs
    where withIs = zip [0..] l
          sortDist l e = l & sortOn (snd &. distsq e)
                           & tail & take n &> fst


timeDist :: (Ord a, Num a) => a -> a -> a -> a
timeDist n t1 t2 = min absDiff (n - absDiff)
  where absDiff = abs (t1 - t2)


rnnBox :: _ => BrainBox NumNeighbhors 1 _ _
rnnBox = buildBrain (initBrain (randWeights 4354) #> (recurrent (biased @3 @3 \> biased @2)) #> biased @3 #> biased)

rnnWeights :: Weights 33
rnnWeights = mkN 1.750423440485752 3.222924885666668 2.2910227257536877 5.439806511011806 4.142005480385802 0.9805584364407558 2.5731630594431865 2.1391747233079625 -3.753164586650063 1.3161119585357113 0.2727157753602446 3.1794925669165823 0.41001096249619695 2.213345108078485 0.2780969665798213 0.9830443442915242 0.7433014784356982 1.6996108574973636 -3.7336393201376086 1.7007999286302289 5.752409698338232 5.2568993666229495 6.0264486640847686e-2 6.986510599213041 5.431577136860181 5.216348090138221 -6.760813283224193 -0.9074726111072304 -0.5494264835463663 -9.957843973110734 -16.751593067894785 -20.83451745186865 -5.652927477333056


reallySmallBox :: _ => BrainBox NumNeighbhors 1 _ _
reallySmallBox = buildBrain (initBrain reallysmallWeights #> (biased @3) #> biased @3 #> biased)

reallysmallWeights :: Weights 34
reallysmallWeights = mkN -15.18170986966183 -3.468838594930856 26.749499493238694 5.369622375702876 28.287233497185504 19.812787024769847 -22.21178484369407 21.870129865061706 4.056926111610593 22.268754057477068 24.91998189703059 34.6774160024471 -24.36860448569042 14.058940908668045 9.420002878804068 -2.0726825294530172e-2 -9.341990499430814 2.676561498447912 -9.543770503562017 0.1780907476439586 8.335392411913825 -53.247911434931325 -6.04425645458969 -10.346201282685383 -4.20987560209897 -17.260992882938744 -4.138928691919586 10.426602561145696 -10.308927139767542 -10.434896816345635 -9.26147116013366 28.562345051310405 12.07769534995233 -26.110517229260317


