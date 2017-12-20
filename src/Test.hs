{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
module Test(Test, testNeuralInstance) where

import Brain
import Convenience
import Minimizer
import Simulator
import Graphics.Gloss
import Control.Monad.Random

type Updater f = [Vec f] -> Vec f -> [Vec f]

newtype Test f = Test ([Vec f],(Vec f, f),Updater f)

instance CanRender Test where
  simRender (Test (points, goal, _)) =
      points &> (\p -> circleSolid 8 & vecTranslate p
                                     & color white)
             & (:) (circleSolid 16 & vecTranslate (computeGoal goal)
                                   & color red)
             & pictures
      where vecTranslate (Vec x y) = translate (realToFrac x) (realToFrac y)

instance Steppable Test where
  simStep (Test (points, goal@(loc, angle) , updater)) = Test (zipWith (+) points vecs, (loc, angle+0.1), updater)
      where vecs = updater points (computeGoal goal)

instance HasCost Test where
  simCost (Test (points, goal, _)) = points &> distsq (computeGoal goal) & sum

instance Default (Test Double) where
  auto = evalRand (randTests (100,100) (applyBeforeBox yes neuralUpdater)) (mkStdGen 114678)

instance Random (Test Double) where
  random = runRand $ randTests (10,10) myUpdater
  randomR = error "no!"

instance Simulator Test where
  -- realToFracSim :: Floating floating => Test Double -> Test floating
  realToFracSim (Test (points, (gp, go), _)) = Test (points &> realToFracVec, (realToFracVec gp,realToFrac go),myUpdater)
    where realToFracVec (Vec x y) = Vec (realToFrac x) (realToFrac y)

computeGoal :: Floating f => (Vec f, f) -> Vec f
computeGoal (loc, angle) = rotateVec (fromScalar 100) angle + loc

randTests :: (RandomGen g) => (Double,Double) -> Updater Double -> Rand g (Test Double)
randTests numTestsRange updater =
  do
    let range = (-500,500)
    randGoal <- getRandomR range
    numTests <- getRandomR numTestsRange &> floor
    points <- getRandomRs range &> take numTests
    return $ Test (points, (randGoal,0), updater)

myUpdater :: _ => f (Vec a) -> Vec a -> f (Vec a)
myUpdater points goal = points &> (\p -> (goal - p)/(dist goal p & fromScalar))

neuralUpdater :: Floating f => Brain 4 2 w f -> Weights w f -> Updater f
neuralUpdater (Brain feed) weights points goal =
    points &> (\p -> vecToSized (p,goal) & feed weights & sizedToVec)
  where
    vecToSized :: (Vec f,Vec f) -> Weights 4 f
    vecToSized (Vec a b, Vec c d) = Sized [a,b,c,d]
    sizedToVec :: Floating f => Weights 2 f -> Vec f
    sizedToVec (Sized [a,b]) = Vec (10*abs a) 0 & flip rotateVec (b*pi)
    sizedToVec _ = error "impossible"

testNeuralInstance :: _ => NeuralSim Test _ _ Double
testNeuralInstance = NeuralSim auto boxWeights restorer neuralSet
    where
        (_,boxWeights,restorer) = yes
        neuralSet :: Floating a => Weights _ a -> Test a -> Test a
        neuralSet weights (Test (a,b,_)) = Test (a,b,neuralUpdater brain weights)
          where (brain, _, _) = yes


-- hmm :: 
-- hmm =
yes :: Floating a => BrainBox 4 2 65 65 a
yes = buildBrain ( initBrain noWeights #> biased @3 #> biased @3 #> biased @3 #> biased @4 #> biased)
yesWeights :: Floating f => Weights 65 f
yesWeights = mkN 1.0478589873652693 -1.2485928663970787 -2.0610049933068617 1.388188988053026 1.939892189875526 -0.3382780636670905 0.9206767919150934 -1.6681630725196726 -0.7622125753189699 1.612530909613983 -1.0644593742426733 2.8363660230738983 0.44627951342585154 -2.9587681571572335 -0.24697223955867917 2.015576165614573 0.24581290009664067 0.9097538477933356 -0.8480698583944422 5.200834481920858e-2 0.6124179368790912 0.9761532982502996 0.30512229752658787 -1.3010311000766615 -1.29459713929744 1.3377330758537325 0.28512013105949274 4.565124582727608 1.7741509028697213 0.9915986804043275 -3.0770654404795597 -0.7418420966281294 -1.6661171483764576 -7.396456519115613 1.3947187483671304 -0.8583639572262435 2.980779351550931 -0.1477736771197551 2.9512568946039153 -1.4039185270616212 -0.30774040835681093 0.1182904807217126 0.8438225744464808 0.40495907539904674 1.421442586651236 1.0592264809442338 -2.0540134506615737 -0.9578852471294674 0.9716371256681041 -0.12217028222723499 0.633025771924017 0.5504753379995238 0.46575312297654176 -0.32421413771386765 1.3173135309316029 -0.45153405873061425 0.25609914833188074 -3.8008781334582387 0.579348937099242 0.8925654483031498 0.30020477274041063 -0.6665123004135518 -0.5450253807097934 1.0652124181860083 0.8515008812565681

noWeights :: Floating f => Weights 65 f
noWeights = randWeights 324234 &> realToFrac