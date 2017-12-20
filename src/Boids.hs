{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Boids(Boids,boidsNeuralInstance) where

import           Brain
import           Control.Lens
import           Convenience
import           Control.Arrow
import           Data.List
import qualified Data.Vector.Storable as V
import           Graphics.Gloss
import           GHC.TypeLits
import           Data.Proxy
import           Minimizer
import           Numeric.FastMath()
import           Simulator
import           Data.KdMap.Static hiding (size,null)
import qualified Control.Monad.Random as R

type Doubles = Vec Double

type Updater (n::Nat) = Doubles -> Sized n (Doubles,Doubles) -> Doubles

type Moves = [(Doubles,Doubles)]

data Boids neighbhors =
  Boids {
    _moves         :: !Moves,
    _goal          :: !(Doubles,Double,Double),
    _size          :: !Int,
    _updater       :: Updater neighbhors
  }
makeLenses ''Boids

numNeighbhors :: forall n x. _ => Boids n -> x
numNeighbhors _ = typeNum (Proxy :: Proxy n)


boidSize :: RealFloat s => s
boidSize = 8

instance CanRender (Boids n) where
  simRender boids =
    colliding (boids^.moves)
    & (\(whites,greens) -> (whites &> fst &. drawCircle white boidSize)
                           ++ (greens &> fst &. drawCircle green boidSize))
    & ((:) (drawCircle red (boidSize*2) (computeGoal (boids^.goal))))
    & pictures
      where vecTranslate (Vec x y) = translate (realToFrac x) (realToFrac y)
            drawCircle c size loc = circleSolid size & vecTranslate loc & color c

instance KnownNat n => Steppable (Boids n) where
  simStep boids@(Boids moves goal@(loc,angle,speed) size updater) =
      Boids newMoves (loc,angle+speed,speed) size updater
    where
      newMoves = moves &> update
      kdm = build vecToList moves
      update (pos,vel) = (pos+vel,0.7*vel + 0.3*updater (computeGoal goal) nClosest)
        where nClosest = kNearest kdm (numNeighbhors boids) pos & fromList

instance HasCost (Boids n) where
  simCost boids = closenessCost + 1000* numCollisions
    where closenessCost = boids^.moves &> fst
                                       &> dist (computeGoal (boids^.goal))
                                       & sum
                                       & (/ fromIntegral (boids^.size))
          numCollisions = colliding (boids^.moves) & fst & length & fromIntegral

instance Default (Boids 4) where
  auto = R.evalRand (randBoids (10,15) (applyBeforeBox tinyBox neuralUpdater) False) (R.mkStdGen 3744253)

instance (x ~ (n+1)) => R.Random (Boids x) where
  random = R.runRand $ randBoids (7,10) myUpdater False
  randomR = error "no!"

colliding :: Moves -> (Moves, Moves)
colliding pointsAndVelocities = pointsAndVelocities & partition (fst &. inRadius kdm (boidSize*2) &. ofLength (1::Int) &. not)
  where kdm = build vecToList pointsAndVelocities

computeGoal :: (Doubles, Double, Double) -> Doubles
computeGoal (loc, angle,_) = rotateVec (fromScalar 400) angle + loc

randBoids :: R.RandomGen g => (Double,Double) -> Updater n -> Bool -> R.Rand g (Boids n)
randBoids numBoidsRange updater singleLine =
  do
    numBoids <- R.getRandomR numBoidsRange
    goalLength <- R.getRandomR (50,300)
    angle <- R.getRandomR (-pi,pi)
    speed <- R.getRandomR (-0.06,0.06)
    let size = floor numBoids
    positions <- getRandomVecs (-20,20)
    let moves = (if singleLine then oneLine numBoids else positions) & addVelocities & take size
    return $ Boids moves (Vec goalLength 0,angle,speed) size updater
  where oneLine numBoids = zipWith Vec (iterate (+16) (-numBoids*8)) (repeat 0)
        addVelocities l = l &> (\x -> (x, Vec 0 0))


myUpdater :: _ => Updater (n+1)
myUpdater goal poss =
  meV * friction + (them &> fst &. flip gravity meP & ssum)*0.7 +
  8*(let grav = gravity meP goal in lerp (-grav) grav (fromScalar (dist meP goal / 230)))
    where (meP,meV) = shead poss
          friction = 0.7
          them = stail poss
          gravity a b = (b-a) / fromScalar (max ((0.05*dist b a)^^(3::Int)) 10)


boidsNeuralInstance :: NeuralSim (Boids 4) _ _
boidsNeuralInstance = NeuralSim auto boxWeights restorer neuralSet
  where
    currentBox@(brain,boxWeights,restorer) = tinyBox
    neuralSet weights = updater .~ (neuralUpdater brain weights)

neuralUpdater :: Brain (n*4+2) 2 w -> Weights w -> Updater n
neuralUpdater (Brain feed) weights goal poss = feed weights cleanedInputs & sizedToVec
  where
    cleanedInputs = goal & vecToSized & joinSized (poss &> moveToSized & sconcat)
    vecToSized :: Vec a -> Sized 2 a
    vecToSized (Vec a b) = Sized [a,b]
    moveToSized :: (Vec a,Vec a) -> Sized 4 a
    moveToSized (Vec a b,Vec c d) = Sized [a,b,c,d]
    sizedToVec :: (Floating a) => Sized 2 a -> Vec a
    sizedToVec (Sized [a,b]) = Vec (20*abs a) 0 & flip rotateVec (b*pi)

brainhicky = initBrain tanhedtiny #> (biased @2 @2 >< recurrent (biased @2 @10 \> biased @2))
                                  #> (biased @4)
                                  #> (biased @4)
                                  #> (biased @4)
                                  #> (biased @2)

tinyBox = buildBrain brainhicky
tanhedtiny = mkN 0.11076548499016407 -1.4682006832350964 1.7848328987689923 0.3505296828753064 -0.12008887029917353 -1.0066063344020455 1.7290198864592896 1.6572208881665704 -1.9031880286040774 1.8059239992282503 -0.8958962068643597 -0.4535180571058606 0.8283611269570865 0.7017082483162445 4.080310175641047 0.37282344250793165 -1.006490391121554 0.9478631799115745 -0.6347885663878372 2.9706954505192655 -1.0863506741773366 2.490152735595471 3.9916797912108013 1.1953776895285988 0.930477685150807 0.9897607137380829 -3.864667147164943 -1.7931167236021504 0.43893556873698997 0.20323713528863435 0.7904004976776484 -0.8134271986397156 -1.2046191688782755 0.35930132476719623 0.7525048256572536 2.7611377837156406e-2 0.1580365095797482 -3.4411114058737615 2.484193016083112 -1.602720175090527 -1.1312178078153714 -3.85347170089576e-2 1.4223403349235657 2.207855003768029 -0.24317209162817366 1.0797348675706449 7.594821936781834e-2 -0.19205458644745804 1.9646217030148465 -0.12986531822338915 0.9236918469229061 0.40391939357921947 1.12621018862982 -1.4713735959886014 1.3038801438286094 -0.7133970157967087 0.7126730845357772 -1.9778129622578016 -0.3312602736781152 -0.2020936721699688 0.7604151600870142 1.916520830801307 1.9878391541759348 -0.8527053781470584 0.5386486986204226 -1.2669312351759592 1.430534068995094 0.5577823271517305 -6.80221482178513e-2 0.11829674981678417 -0.9416491191717178 2.9270910918146837 0.6756418142768129 2.0039921108668337 0.25764741115196776 -0.7417098116635916 0.8787054845013503 1.7368738676619473 0.2094218402597499 0.4049768845483419 0.1408799502036004 -3.372840167986611e-2 1.1032558061082813 0.19497198150749795 0.12415580689442746 0.3678821263359829 0.45129707630107796 1.4332133705516743 -0.15498038549350418 0.1342511663788512 -0.10655401221753427 0.5270451918711982 0.1875820075706172 0.6603854767031596 1.2582948807915755 -3.3985346910861183 -0.1511464660154645 -0.2702031716122121 -0.20060472535755963 -0.8857769679320546 -0.573031516801678 -0.9332627311396324 -0.2382921045787687 5.092408660039194e-4


stupidBox = buildBrain (initBrain tanhedB #> biased @4
                                          #> biased @4
                                          #> biased @4
                                          #> biased @2)
tanhedB :: Weights 126
tanhedB = randWeights 234234

simpleBox = buildBrain (initBrain tanhsim #> biased @4
                                          #> biased @2)
tanhsim :: Weights 86
tanhsim = randWeights 245544
