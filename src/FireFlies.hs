{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NegativeLiterals #-}
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
import           Data.Proxy
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

    mainState = randFlies (100,100) 203430 5 (applyBeforeBox reallySmallBox neuralSyncer) True


type NumNeighbhors = 5

fliesNeuralSimInstance :: (RealFloat a,Ord a,V.Storable a) =>
                              NeuralSim (Flies a) a _ _ _ _
fliesNeuralSimInstance = NeuralSim fliesSimulatorInstance currentBox randTrainingState neuralStep
  where
    currentBox@(brain,_,_) = reallySmallBox
    numNeighbhors = typeNum (Proxy :: Proxy NumNeighbhors)

    neuralStep system weights =
      _simStep fliesSimulatorInstance (system & syncer .~ (neuralSyncer brain weights (system^.period)))
    randTrainingState seed weights =
        randFlies (100,100) (seed+1) numNeighbhors (neuralSyncer brain weights) True


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
rnnBox = buildBrain (initBrain (randWeights 4354) #> (recurrent (biased @3 @3 \> biased @2)) #> biased @3 #> biased)

rnnWeights :: (RealFloat a, V.Storable a) => Weights 33 a
rnnWeights = mkN 1.750423440485752 3.222924885666668 2.2910227257536877 5.439806511011806 4.142005480385802 0.9805584364407558 2.5731630594431865 2.1391747233079625 -3.753164586650063 1.3161119585357113 0.2727157753602446 3.1794925669165823 0.41001096249619695 2.213345108078485 0.2780969665798213 0.9830443442915242 0.7433014784356982 1.6996108574973636 -3.7336393201376086 1.7007999286302289 5.752409698338232 5.2568993666229495 6.0264486640847686e-2 6.986510599213041 5.431577136860181 5.216348090138221 -6.760813283224193 -0.9074726111072304 -0.5494264835463663 -9.957843973110734 -16.751593067894785 -20.83451745186865 -5.652927477333056


reallySmallBox :: _ => BrainBox t NumNeighbhors 1 _ _
reallySmallBox = buildBrain (initBrain reallysmallWeights #> (biased @3) #> biased @3 #> biased)

reallysmallWeights :: (RealFloat a, V.Storable a) => Weights 34 a
reallysmallWeights = mkN -21.147765925861748 -7.157520810353779 21.867200558295607 3.1873027120123423 23.153808797252992 19.953684577169447 -13.931980859957292 19.690257687079672 -0.725098590502125 16.864812826017662 24.388280078155965 36.565231025605044 -24.763620768975855 12.380198264778645 10.061059717637573 -0.8041508421257408 -9.97285236309175 0.7930357200543705 -9.71132095994771 0.212352901695059 8.168080838848994 -55.31368141505385 -6.433411124149906 -13.11363109954484 -2.044228629855218 -3.2491047847162804 -5.378954435521983 10.620746019167061 -11.492523001566003 -8.824140419183614 -6.8609385938675995 30.279113023315666 6.655981326763193 -24.438219318848198
