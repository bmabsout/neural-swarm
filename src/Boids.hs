{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Boids(Boids, boidsSimulatorInstance,boidsNeuralInstance) where

import           Brain
import           Control.Lens
import           Convenience
import           Data.Bifunctor
import           Data.List
import qualified Data.Vector.Storable as V
import           Graphics.Gloss
import           GHC.TypeLits
import           Minimizer
import           Numeric.FastMath()
import           Simulator
import           Data.KdMap.Static hiding (size)

type Updater (n::Nat) a = Vec a -> Sized n (Vec a,Vec a) -> Vec a

data Boids neighbhors a =
  Boids {
    _moves         :: [(Vec a,Vec a)],
    _goal          :: (Vec a,a),
    _numBumps      :: a,
    _size          :: Int,
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

        mainState = randBoids (10,15) 13456 (applyBeforeBox complexerBox neuralUpdater) False

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
                              NeuralSim (Boids _ a) a _ _
boidsNeuralInstance = NeuralSim boidsSimulatorInstance currentBox randTrainingState
  where
    currentBox@(brain,_,_) = complexerBox
    randTrainingState seed weights =
      randBoids (7,7) (seed+1) (neuralUpdater brain weights) False

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

singleBrain :: _ => Brain a _ _ _
singleBrain = biased @6 \> biased @2

singleTrained :: (Floating a,V.Storable a) => Weights 56 a
singleTrained = mkN 2.878362078203081 2.034898115931643 4.592344229237551e-2 2.32732784469882 -0.15894765077964532 -2.028659308975036 -4.460681081385462e-2 4.245004200167297 5.489340973981703e-2 -1.5703042670114022 -0.9384888090595965 -7.579488197960818e-2 -0.4169269570765018 1.2021184853830658 0.8596312329734084 0.8805624139216299 -1.4939308017333275 1.911980835892805 -0.4948862652923467 -3.241409114291259 1.5332170536014673 -0.32680363598054707 -1.8807876613307064 0.3194127080821002 1.5381848646119745 0.37175608690274775 1.0890431598157482 -0.5419620353006017 1.9857162910031867 2.7428641292265354e-2 -1.6370964572858528 -0.8762959213260318 1.5380998067416884 -0.2596843861608733 1.436656487629738 -0.8018431189343656 -3.7623755287996183 0.11052479896333106 -1.3062817949666672 0.9583461776147085 3.7767571697671696 -0.15744673730839143 0.20482544779579825 -6.275614835255794 -1.1197018708820863 -0.6853103021472594 3.438156375511079 0.2626208318912638 2.669601537553346 -1.3721546153769162 -0.9875599558433481 4.574343537705992 1.6147913870609147 -1.0754092383702685 4.860173271404291 0.17016999016969553

stupidBrain :: _ => Brain a _ _ _
stupidBrain = biased @10 \> biased @10 \> biased @2

simpleBrain :: _ => Brain a _ _ _
simpleBrain = biased @8 \> biased @2

complexBrain :: _ => Brain a _ _ _
complexBrain = (biased @2 @2 >< shared Proxy inputizer) \> biased @2
  where
    inputizer = (biased @4 @2 \> biased @1)

complexerBox :: _ => BrainBox a _ _ _ _
complexerBox = buildBrain (initBrain complexerTrained #> (biased @2 @2 >< shared Proxy inputizer)
                                                      #> (biased @4)
                                                      #> (biased @2))
  where inputizer = (biased @4 @2 \> biased @1)


complexTrained :: (Floating a) => Weights 45 a
complexTrained = mkN 0.9337534408534242 -6.534352197890153e-2 -1.3888183694190803 1.4313673974785859 1.4268979573441052 0.8108559673550602 -0.38619040439454766 0.5088399372205417 -0.7736190878170282 1.751028096939815 -0.15045216131169398 1.5075170239042353 -0.18726761272280976 0.5156911350229445 0.6780185156488028 8.279834972995109e-2 0.8617736471469392 -1.255194103273179 -1.3736258068743408 2.239918042728295 0.373337815752257 -0.3010839260088204 1.5151782775776432 -0.24413859624083184 1.444023695742562 -2.7319016105066125 -0.27193290077149496 0.2877544953999117 -0.5779270205945319 -1.3767378301841902 0.30256133180190936 -0.4739494408289684 -0.26022258301544565 -0.8221539908095947 -2.2974185640521805 2.141150560102847 -0.4281648145769912 -0.9457819335089815 0.9883052917173846 0.563722212862495 0.3526707530965876 0.9597878483647071 1.4448240466171762 -0.6277374252170365 0.44477913931109303

complexerTrained :: (RealFloat a) => Weights 77 a
complexerTrained =mkN 1.740308366769931 0.4489899455805406 0.843114109910376 1.5765720511813548 0.8600503986622832 3.6726697193739657 0.5531080237513899 -3.6906807654229974 -2.3918632934709194 6.18751291864581 -2.6644412497887355 -1.818902904743382 0.25793144349053465 3.999990104023553 -3.414267379061891 -1.149359858697653 -1.8537102678473611 6.360907368529151 -1.8193422195724072 1.6083224663776385 4.657707438704508 3.0411951017063545 0.3856070922259336 2.6572386621023885 -0.9659749863294873 -0.19223061038293032 1.8206691110057944 0.5186002039274042 1.8416653935449707 1.7078614367171325 1.0741170188832396 0.41726279831275703 2.322572339122291 1.2453988532976266 0.18797933506476755 -3.739870322004672e-2 0.7774729633279065 0.2198593255232576 1.5191563536071633 0.5279149960597886 -0.13091983399215584 1.877083920364674 0.41113619791150047 0.1965957034734595 -3.2024743084040325e-2 0.45251680076051015 1.8595339025573816 1.1633395493627847 0.39205218669453656 -3.0093580213403426 0.7663127505073688 -0.6569308610239772 0.9183704860306439 -0.5495254217155556 0.23498427276959477 1.1385205089161543 -0.32565720603297843 1.8961144231639682 0.26280510064692175 -1.5185102177929464 3.5546228518087384 -1.3935282307571333 1.9344949816216976 -0.3471523743639138 0.607172662882226 -1.066086559763617 -0.8419237496690473 -0.15650015969318415 -1.8326214418624769 0.4998948615178427 0.660368057068135 0.925683778639393 -1.9366279478768562 0.4000381211777161 2.974935976162338 -0.37900159549764467 -1.1950835852276516
