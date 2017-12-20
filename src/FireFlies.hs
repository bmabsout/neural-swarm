{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NegativeLiterals #-}
module FireFlies
    ( Flies,
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
import qualified Control.Monad.Random as R

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


instance HasCost Flies where
    simCost (Flies p n ts _ s m _) = m &> newGetIxs (V.fromList ts)
                                       &. allWithAll p (fromIntegral n)
                                       & sum & (/ fromIntegral s) & realToFrac & log
        where allWithAll p s l = l &> (\a -> l &> (\b ->timeDist p a b) & sum)
                                   & sum & (/ s**2)
instance CanRender Flies where
    simRender fs = zipWith (renderFirefly (fs^.period)) (fs^.positions) (fs^.times)
                   & pictures
        where
            renderFirefly :: RealFloat a => a -> Vec a -> a -> Picture
            renderFirefly period pos time = circleSolid 8 & vecTranslate pos
                                                          & color currentColor
                where vecTranslate (Vec x y) = translate (realToFrac x) (realToFrac y)
                      currentColor = let t = realToFrac (time / period)
                                     in makeColor t t t 1

instance Steppable Flies where
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

instance Default Flies where
  auto = R.evalRand (randFlies (15,15) 5 (applyBeforeBox reallySmallBox neuralSyncer) False) (R.mkStdGen 23434)

instance R.Random Flies where
  random = R.runRand $
    randFlies (15,15) (typeNum (Proxy @NumNeighbhors)) mySyncer False
  randomR = error "no!"

type NumNeighbhors = 5

fliesNeuralInstance :: NeuralSim Flies _ _
fliesNeuralInstance = NeuralSim auto boxWeights restorer setWeights
  where
    currentBox@(brain,boxWeights,restorer) = reallySmallBox
    setWeights weights system = system & syncer .~ (neuralSyncer brain weights (system^.period))


newGetIxs vec indices = indices & V.fromList & V.map fromIntegral
                                & V.backpermute vec & V.toList


neuralSyncer :: _ => Brain _ _ w -> S w a -> a -> Syncer
neuralSyncer (Brain feedForward) weights period inputs = feedForward weights (fromList inputs) & shead & (+1) & (*0.5) & (*(-period))

mySyncer :: Double -> Syncer
mySyncer period l = l &> ringDiff period & mean
  where ringDiff n t = t + if t > n/2 then -n+t else t
        mean [] = 0
        mean l  = sum l / fromIntegral (length l)

randFlies :: R.RandomGen g => (Double,Double) -> Int -> (Double -> Syncer) -> Bool -> R.Rand g Flies
randFlies numFliesRange numNeighbhors syncer singleLine =
  do
    numFlies <- R.getRandomR numFliesRange
    let size = floor numFlies
    let per = 0.5
    ts <- R.getRandomRs (0,per) &> take size
    randomPositions <- getRandomVecs (-500,500)
    let vecs = (if singleLine
                then zipWith Vec (iterate (+16) (-numFlies*8)) (repeat 0)
                else randomPositions) & take size
    return $ Flies per numNeighbhors ts (vecs) size (closestIs numNeighbhors vecs) (syncer per)



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
reallySmallBox = buildBrain (initBrain tanhed #> (biased @3) #> biased @3 #> biased)

please :: Weights _
please = mkN -14.080851497440872 -1.0351226841936723 10.86138758965171 28.809506819931947 4.1264639317508145 13.660051079343354 5.1128048131202615 -15.271050087543095 3.9438373544214667 -30.413427987460352 -5.6296050253582575 11.805677646165925 1.1695156229384156 -0.19513673120973007 6.261899620799466e-2 9.508371567260035 -0.5637197000288356 -0.22035630138535067 3.6512652201906626 -9.82019053922675 -0.15067288443591265 17.1331914976272 -2.6277264378444114 44.69340895693824 2.575174891726534 4.112543902082365 2.7579883855036407 10.554378390336684 -2.6719206892467815 -14.277847210500102 3.9854274807485304 -8.977327203031749 7.328396233280756 7.486410575168472


reallysmallWeights :: Weights 34
reallysmallWeights = mkN -15.18170986966183 -3.468838594930856 26.749499493238694 5.369622375702876 28.287233497185504 19.812787024769847 -22.21178484369407 21.870129865061706 4.056926111610593 22.268754057477068 24.91998189703059 34.6774160024471 -24.36860448569042 14.058940908668045 9.420002878804068 -2.0726825294530172e-2 -9.341990499430814 2.676561498447912 -9.543770503562017 0.1780907476439586 8.335392411913825 -53.247911434931325 -6.04425645458969 -10.346201282685383 -4.20987560209897 -17.260992882938744 -4.138928691919586 10.426602561145696 -10.308927139767542 -10.434896816345635 -9.26147116013366 28.562345051310405 12.07769534995233 -26.110517229260317


tanhed :: Weights _
tanhed = mkN 3.7238029189993616 -3.475245020438038 -9.692364045372943e-2 -4.009637440957896 -3.82127028826974 0.16593670077656644 0.6169153303742647 0.42019091420908855 0.1660307923214792 0.49464966684020073 5.138080252180406e-2 0.11339081710082474 0.3292300899087112 2.8663675247833114 0.1934281883730344 3.1606551415132604 3.3678007577422064e-2 8.369876295418474e-2 -1.397604274832784 1.6530932501825175 0.7174063413151947 2.334965421863858 0.446904883979963 2.1219410660326754 1.150742494644102 2.4450649701697413 0.19975980078601627 0.25702530502151505 -1.441784029097739 0.5575806237907395 1.4373009905254988 0.8800629111947817 -2.1597571816627466 -1.9309050027487862

