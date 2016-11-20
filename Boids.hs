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

data Boids neighbhors a b =
  Boids {
    _moves         :: [(Vec a,Vec a)],
    _goal          :: (Vec a,a),
    _numBumps      :: a,
    _size          :: b,
    _updater       :: Updater neighbhors a
  }
makeLenses ''Boids

numNeighbhors :: forall n a b x. _ => Boids n a b -> x
numNeighbhors _ = typeNum (Proxy :: Proxy n)

boidsSimulatorInstance :: forall a b . (RealFloat a,Integral b,V.Storable a) => Simulator (Boids 4 a b) a
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
            Boids newMoves (loc,angle+0.1) newNumBumps size updater
          where
            (newMoves, newNumBumps) = moves &> update & unzip & second (sum &. (+numBumps))
            kdm = buildWithDist vecToList distsq moves
            update (pos,vel) = ((pos+vel,updater (computeGoal goal) nClosest), numCollisions)
              where
                numCollisions = (inRadius kdm boidSize pos & genericLength) - 1
                nClosest = kNearest kdm (numNeighbhors boids) pos & fromList

        simCost boids = closenessCost + 0.1*boids^.numBumps
          where closenessCost = boids^.moves &> fst
                                             &> distsq (computeGoal (boids^.goal))
                                             & sum
                                             & (/ fromIntegral (boids^.size))

        mainState = randBoids (300,300) 133435 (neuralUpdater complexerBrain complexerTrained) False

computeGoal :: Floating a => (Vec a, a) -> Vec a
computeGoal (loc, angle) = rotateVec (fromScalar 100) angle + loc

randBoids :: (RealFloat a,Integral b) => (a,a) -> a -> Updater n a -> Bool -> Boids n a b
randBoids numBoidsRange seed updater singleLine =
    Boids moves (goal,0) 0 size updater
    where
      goal = pseudoRands (-500,500) (seed+4) & (\(a:b:_) -> Vec (a,b))
      size = floor numBoids
      numBoids = pseudoRand numBoidsRange (seed+3)
      xgen | singleLine = iterate (+16) (-numBoids*8)
           | otherwise  = pseudoRands (-500,500) (seed+2)

      ygen | singleLine = repeat 0
           | otherwise  = pseudoRands (-500,500) (seed+1)
      moves = zipWith (curry Vec) xgen ygen & flip zip (repeat $ Vec (0,0)) & genericTake size


myUpdater :: _ => Updater (n+1) a
myUpdater goal poss =
  meV * friction + (them &> fst &. flip gravity meP & ssum)*0.7 +
  8*(let grav = gravity meP goal in lerp (-grav) grav (fromScalar (dist meP goal / 230)))
    where (meP,meV) = shead poss
          friction = 0.7
          them = stail poss
          gravity a b = (b-a) / fromScalar (max ((0.05*dist b a)^^(3::Int)) 10)


boidsNeuralInstance :: (RealFloat a,Ord a,V.Storable a,V.Storable b,Integral b) =>
                              NeuralSim (Boids _ a b) a _
boidsNeuralInstance = NeuralSim boidsSimulatorInstance complexerTrained randTrainingState
  where
    randTrainingState seed weights =
      randBoids (5,10) (seed+1) (neuralUpdater complexerBrain weights) False

neuralUpdater :: (Num a) => Brain a (n*4+2) 2 w -> Weights w a -> Updater n a
neuralUpdater (Brain feed) weights goal poss = feed weights cleanedInputs & sizedToVec
  where
    cleanedInputs = goal & vecToSized & joinSized (poss &> moveToSized & sconcat)
    vecToSized :: Vec a -> Sized 2 a
    vecToSized (Vec (a,b)) = Sized [a,b]
    moveToSized :: (Vec a,Vec a) -> Sized 4 a
    moveToSized (Vec (a,b),Vec (c,d)) = Sized [a,b,c,d]
    sizedToVec :: (Num a) => Sized 2 a -> Vec a
    sizedToVec (Sized [a,b]) = Vec (lerp -10 10 a,lerp -10 10 b)

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

complexerBrain :: _ => Brain a _ _ _
complexerBrain = (biased @2 @2 >< shared Proxy inputizer) \> biased @4 \> biased @2
  where
    inputizer = (biased @4 @2 \> biased @1)



complexTrained :: (Floating a) => Weights 45 a
complexTrained = mkN 0.9337534408534242 -6.534352197890153e-2 -1.3888183694190803 1.4313673974785859 1.4268979573441052 0.8108559673550602 -0.38619040439454766 0.5088399372205417 -0.7736190878170282 1.751028096939815 -0.15045216131169398 1.5075170239042353 -0.18726761272280976 0.5156911350229445 0.6780185156488028 8.279834972995109e-2 0.8617736471469392 -1.255194103273179 -1.3736258068743408 2.239918042728295 0.373337815752257 -0.3010839260088204 1.5151782775776432 -0.24413859624083184 1.444023695742562 -2.7319016105066125 -0.27193290077149496 0.2877544953999117 -0.5779270205945319 -1.3767378301841902 0.30256133180190936 -0.4739494408289684 -0.26022258301544565 -0.8221539908095947 -2.2974185640521805 2.141150560102847 -0.4281648145769912 -0.9457819335089815 0.9883052917173846 0.563722212862495 0.3526707530965876 0.9597878483647071 1.4448240466171762 -0.6277374252170365 0.44477913931109303

complexerTrained :: (Floating a) => Weights 77 a
complexerTrained = mkN 0.7498546604416285 -1.9580910660518582 0.35472160371443867 -3.144623857705083e-2 -0.8276637965260194 0.4978092631515776 -1.1787397322682773 1.145860639128185 -1.1678083770441439 1.4327970432992791 -0.41459115860268114 -0.9219539310627243 0.5107389421990649 0.6164112708703142 -1.155635481733971e-2 -1.664046240263338e-2 -0.2971092633985348 1.9377895379592074 1.8764432022581073e-2 3.5267782770706293 1.8175815079842756 0.5003842931414194 -2.365359268354296 1.3882000207351324 -1.063925022331282 -0.5102801954345404 0.5792343266900676 0.3339402555998455 0.6898484103610004 2.1304729749353726 0.5101283324251098 0.10122064740495804 1.151312928562291 0.367902988390427 0.7554502257786888 0.974564814642575 1.6453004161582119 -0.1425249101867 -0.7827426574889088 0.1441483009818154 -0.733813339863681 0.9020299542177668 0.15650099328296524 0.6851785922010384 -0.9114618367829541 0.6894106107645686 1.7739808949914 0.918503933984263 -0.5107335481765529 -2.1533384550157644 0.23609626544904522 -0.34575530088305173 -0.11964766569779649 -1.6657148723833095 6.58992506438569e-2 0.7010667194403869 -0.7447057385306781 2.195920971078468 0.9481582505470882 -0.5779438723055812 3.443691574250098 -0.7789915287149887 2.690813486406226 -1.0736480243217592 0.1934801875742795 -0.4393582404068627 -0.5462892204682763 -0.6462518255440356 -1.0328743050601934 0.6673512888577428 1.935179362518324 1.6483204266776204 -0.12910513621812525 0.642100010616568 -0.10435646567014172 -1.0656990071612937 1.6087997899667927
