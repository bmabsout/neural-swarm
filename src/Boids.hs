{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
module Boids(Boids, boidsSimulatorInstance,boidsNeuralInstance) where

import           Brain
import           Control.Lens
import           Convenience
import           Data.Bifunctor
import           Data.List
import qualified Data.Vector.Storable as V
import           Graphics.Gloss
import           GHC.TypeLits
import           Data.Proxy
import           Minimizer
import           Numeric.FastMath()
import           Simulator
import           Data.KdMap.Static hiding (size)

type Updater (n::Nat) a = Vec a -> Sized n (Vec a,Vec a) -> Vec a

data Boids neighbhors a =
  Boids {
    _moves         :: ![(Vec a,Vec a)],
    _goal          :: !(Vec a,a),
    _numBumps      :: !a,
    _size          :: !Int,
    _updater       :: Updater neighbhors a
  }
makeLenses ''Boids

numNeighbhors :: forall n a x. _ => Boids n a -> x
numNeighbhors _ = typeNum (Proxy :: Proxy n)

boidsSimulatorInstance :: (RealFloat a, V.Storable a) => Simulator (Boids 4 a) a
boidsSimulatorInstance = Simulator simRender simStep simCost mainState
    where
        boidSize :: RealFloat s => s
        boidSize = 8
        simRender boids =
            boids^.moves &> (\(p,_) -> circleSolid boidSize & vecTranslate p
                                                            & color white)
                         & ((:) (circleSolid (boidSize*2) & vecTranslate (computeGoal (boids^.goal))
                                                          & color red))
                         & pictures
            where vecTranslate (Vec (x,y)) = translate (realToFrac x) (realToFrac y)

        simStep boids@(Boids moves goal@(loc,angle) numBumps size updater) =
            Boids newMoves (loc,angle+0.06) newNumBumps size updater
          where
            (newMoves, newNumBumps) = moves &> update & unzip & second (sum &. (+numBumps))
            kdm = buildWithDist vecToList distsq moves
            update (pos,vel) = ((pos+vel,updater (computeGoal goal) nClosest), numCollisions)
              where
                numCollisions = (inRadius kdm boidSize pos & length & fromIntegral) - 1
                nClosest = kNearest kdm (numNeighbhors boids) pos & fromList


        simCost boids = closenessCost + 0.001*boids^.numBumps
          where closenessCost = boids^.moves &> fst
                                             &> distsq (computeGoal (boids^.goal))
                                             & sum
                                             & (/ fromIntegral (boids^.size))

        mainState = randBoids (10,15) 135467 (applyBeforeBox tinyBox neuralUpdater) False



computeGoal :: Floating a => (Vec a, a) -> Vec a
computeGoal (loc, angle) = rotateVec (fromScalar 400) angle + loc

randBoids :: RealFloat a => (a,a) -> a -> Updater n a -> Bool -> Boids n a
randBoids numBoidsRange seed updater singleLine =
    Boids moves (goal,0) 0 size updater
    where
      goal = pseudoRands (-1000,1000) (seed+4) & (\(a:b:_) -> Vec (a,b))
      size = floor numBoids
      numBoids = pseudoRand numBoidsRange (seed+3)
      xgen | singleLine = iterate (+16) (-numBoids*8)
           | otherwise  = pseudoRands (-200,200) (seed+2)

      ygen | singleLine = repeat 0
           | otherwise  = pseudoRands (-200,200) (seed+1)
      moves = zipWith (curry Vec) xgen ygen & flip zip (repeat $ Vec (0,0)) & take size


myUpdater :: _ => Updater (n+1) a
myUpdater goal poss =
  meV * friction + (them &> fst &. flip gravity meP & ssum)*0.7 +
  8*(let grav = gravity meP goal in lerp (-grav) grav (fromScalar (dist meP goal / 230)))
    where (meP,meV) = shead poss
          friction = 0.7
          them = stail poss
          gravity a b = (b-a) / fromScalar (max ((0.05*dist b a)^^(3::Int)) 10)


boidsNeuralInstance :: (RealFloat a,Ord a,V.Storable a) =>
                              NeuralSim (Boids _ a) a _ _ _ _
boidsNeuralInstance = NeuralSim boidsSimulatorInstance currentBox randTrainingState neuralStep
  where
    currentBox@(brain,_,_) = tinyBox
    neuralStep boids weights = _simStep boidsSimulatorInstance (boids & updater .~ (neuralUpdater brain weights))
    randTrainingState seed weights =
      randBoids (7,10) (seed+1) (neuralUpdater brain weights) False

neuralUpdater :: (Num a) => Brain a (n*4+2) 2 w -> Weights w a -> Updater n a
neuralUpdater (Brain feed) weights goal poss = feed weights cleanedInputs & sizedToVec
  where
    cleanedInputs = goal & vecToSized & joinSized (poss &> moveToSized & sconcat)
    vecToSized :: Vec a -> Sized 2 a
    vecToSized (Vec (a,b)) = Sized [a,b]
    moveToSized :: (Vec a,Vec a) -> Sized 4 a
    moveToSized (Vec (a,b),Vec (c,d)) = Sized [a,b,c,d]
    sizedToVec :: (Num a) => Sized 2 a -> Vec a
    sizedToVec (Sized [a,b]) = Vec (lerp -40 40 a,lerp -40 40 b)


complexerBox :: _ => BrainBox a _ _ _ _
complexerBox = buildBrain (initBrain complexerTrained #> (biased @2 @2 >< shared inputizer)
                                                      #> (biased @4)
                                                      #> (biased @2))
  where inputizer = (biased @4 @2 \> biased @1)

complexerTrained :: (RealFloat a) => Weights 77 a
complexerTrained =mkN 1.740308366769931 0.4489899455805406 0.843114109910376 1.5765720511813548 0.8600503986622832 3.6726697193739657 0.5531080237513899 -3.6906807654229974 -2.3918632934709194 6.18751291864581 -2.6644412497887355 -1.818902904743382 0.25793144349053465 3.999990104023553 -3.414267379061891 -1.149359858697653 -1.8537102678473611 6.360907368529151 -1.8193422195724072 1.6083224663776385 4.657707438704508 3.0411951017063545 0.3856070922259336 2.6572386621023885 -0.9659749863294873 -0.19223061038293032 1.8206691110057944 0.5186002039274042 1.8416653935449707 1.7078614367171325 1.0741170188832396 0.41726279831275703 2.322572339122291 1.2453988532976266 0.18797933506476755 -3.739870322004672e-2 0.7774729633279065 0.2198593255232576 1.5191563536071633 0.5279149960597886 -0.13091983399215584 1.877083920364674 0.41113619791150047 0.1965957034734595 -3.2024743084040325e-2 0.45251680076051015 1.8595339025573816 1.1633395493627847 0.39205218669453656 -3.0093580213403426 0.7663127505073688 -0.6569308610239772 0.9183704860306439 -0.5495254217155556 0.23498427276959477 1.1385205089161543 -0.32565720603297843 1.8961144231639682 0.26280510064692175 -1.5185102177929464 3.5546228518087384 -1.3935282307571333 1.9344949816216976 -0.3471523743639138 0.607172662882226 -1.066086559763617 -0.8419237496690473 -0.15650015969318415 -1.8326214418624769 0.4998948615178427 0.660368057068135 0.925683778639393 -1.9366279478768562 0.4000381211777161 2.974935976162338 -0.37900159549764467 -1.1950835852276516


tinyBox :: _ => BrainBox a _ _ _ _
tinyBox = buildBrain (initBrain theSmartOnes #> (biased @2 @2 >< recurrent (biased @2 @10 \> biased @2))
                                                  #> (biased @8)
                                                  #> (biased @2))


theSmartOnes :: _ => Weights _ _
theSmartOnes = mkN -43.97322373371871 4.121208170426731 -10.569897314198064 -21.379790850608735 6.012564616695833 -4.745508003646624 -13.908537305903534 16.680213497758903 14.78007193531118 -6.212941037110924 45.04742677381116 32.68860506262502 23.982702059604225 2.461175096241873 31.664564357967492 12.087662749306345 56.52438749166996 -22.514763184945426 4.632542351228821 -26.62029587546922 -21.08687290797834 17.525905944176802 -42.53516069951659 8.360790141907264 15.912524360735478 -8.9610691517738 45.97344582961509 -44.19563149898908 18.516220499679708 -12.151438383077197 -6.832138533350091 2.182517331547834 14.434189946544109 47.82355037215911 23.58100349797017 14.501392879382642 -26.799411905946037 -6.027777255838845 -16.56465354205036 17.720167171217774 -10.483638898987357 12.534419520924681 -22.528127130214955 -16.315358056407966 41.841154697553584 12.84100831600406 11.143536061127826 -6.740476090591725 -3.1957750151905695 38.37331603668877 -12.905504854471925 0.4061469162120392 -2.212687641502587 7.341318052489545 -13.953336055638308 1.9488685782641846 28.228744630845068 0.17097400142858932 23.700862987032608 8.810668337269465 47.016907805529435 -7.6887575865580935 -4.725461656670311 -12.8439514437354 -7.1435421063551985 24.225326134060428 -30.945877737911903 13.825258353339633 9.820795129664061 33.3132600709325 20.434646276841548 -24.46811378105894 -3.11443142910787 -19.973225383847033 2.999299812522044 25.862272861264298 4.541208980978038 43.07991123525539 41.01278585237007 -30.50477741989265 -8.271345262954185 1.2213085246001847 -17.91855791267463 -21.82885690582837 50.01588261225725 -6.899512417230965 17.644464921102948 36.30535416689151 -37.2371478692004 22.48338900881082 0.3520193567053915 39.68933129183484




tinyWeights :: RealFloat a => Weights 76 a
tinyWeights = mkN 3.922315476597114 -0.13450519920872284 -2.3962746679585916 0.10321053519141246 0.18534963486536793 2.956230607050921 0.7336625979165361 1.6046958846280406 1.938238197248392 3.4568168380058015 -0.7263030161385771 -1.7619060564882272 0.627410912250933 5.4120391300911574 1.8701786394539637 7.109160648003561 2.24015028116634 1.6285811014233689 1.5509144108822275 2.4523602988192943 0.6118352077961706 1.9856608763078487 0.5775432294743216 1.8195281828754304 -0.8590003560512598 2.77934643560699 2.33771579152015 1.012016010932021 -12.436588724605173 1.4921972215819377 0.8366579736434048 0.5656484675045679 3.364873352999945 -1.6444782726344207 0.764312221470826 0.5434276198703405 7.421745953195463e-2 -1.8111097601965636 -2.6251835384500763 -0.39380806237901633 0.8575400329129428 0.8927484119793694 -5.965266445054212 -1.0337820545436425 -2.7355275500481856e-2 -1.0939002290437698e-2 -0.40388754683070716 -0.6597285114756088 0.4283368809276401 0.5456829293348073 1.264901217157293 -0.427081859191617 -0.6162706900519654 0.3036675139849113 0.559798978374672 1.27236713787133 -0.3736626074075923 0.30861952458887454 2.3528177217065247 0.17318751567606516 1.8179323810524615 1.121907747882216e-2 -0.7770078951781978 0.3282583782988119 0.2872454216623678 0.6995654813233056 -0.31268236410147754 3.603445099769534e-2 1.180386417995785 6.374111552085099e-2 -0.9153682250342798 -0.3674260827242092 -0.32336266963861504 -0.23628657084330768 0.6062547227792376 0.2298495048334505
