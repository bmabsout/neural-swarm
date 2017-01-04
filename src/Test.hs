{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
module Test(Test, testSimulatorInstance, testNeuralInstance) where

import Brain
import Convenience
import Minimizer
import Simulator
import Graphics.Gloss

type Updater a = [Vec a] -> Vec a -> [Vec a]

newtype Test a = Test ([Vec a],(Vec a, a),Updater a)

testSimulatorInstance :: RealFloat a => Simulator (Test a) a
testSimulatorInstance = Simulator simRender simStep simCost mainState
    where
        simRender (Test (points, goal, _)) =
            points &> (\p -> circleSolid 8 & vecTranslate p
                                           & color white)
                   & (:) (circleSolid 16 & vecTranslate (computeGoal goal)
                                         & color red)
                   & pictures
            where vecTranslate (Vec (x,y)) = translate (realToFrac x) (realToFrac y)

        simStep (Test (points, goal@(loc, angle) , updater)) = Test (zipWith (+) points vecs, (loc, angle+0.1), updater)
            where vecs = updater points (computeGoal goal)

        simCost (Test (points, goal, _)) = points &> distsq (computeGoal goal) & sum

        mainState = randTests (100,100) (applyBeforeBox box neuralUpdater) 114678

computeGoal :: Floating a => (Vec a, a) -> Vec a
computeGoal (loc, angle) = rotateVec (fromScalar 100) angle + loc

randTests :: RealFloat a => (a,a) -> Updater a -> a -> Test a
randTests numTestsRange updater seed = Test (points, (randGoal,0), updater)
    where range = (-500,500)
          randGoal = Vec (pseudoRand range seed, pseudoRand range (seed+1))
          numTests = pseudoRand numTestsRange (seed+2) & floor
          points = zipWith (curry Vec)
                           (pseudoRands range (seed+3))
                           (pseudoRands range (seed+4))
                   & take numTests

myUpdater points goal = points &> (\p -> (goal - p)/(dist goal p & fromScalar))

neuralUpdater :: (Num a) => Brain a 4 2 w -> Weights w a -> Updater a
neuralUpdater (Brain feed) weights points goal =
    points &> (\p -> vecToSized (p,goal) & feed weights & sizedToVec)
  where
    vecToSized :: (Vec a,Vec a) -> Sized 4 a
    vecToSized (Vec (a,b), Vec (c,d)) = Sized [a,b,c,d]
    sizedToVec :: Num a => Sized 2 a -> Vec a
    sizedToVec (Sized [a,b]) = Vec (lerp -10 10 a, lerp -10 10 b)

testNeuralInstance :: RealFloat a => NeuralSim (Test a) a _ _ _ _
testNeuralInstance = NeuralSim testSimulatorInstance currentBox randTrainingState neuralStep
    where
        currentBox@(brain,_,_) = box
        neuralStep (Test (a,b,_)) weights = _simStep testSimulatorInstance (Test (a,b,neuralUpdater brain weights))
        randTrainingState seed weights =
            randTests (10,10) (neuralUpdater brain weights) (seed+1)

box :: _ => BrainBox a _ _ _ _
box = buildBrain (initBrain bamboo #> (Disable (biased @3)) #> biased @10 #> biased @2)


bamboo :: _ => Weights _ _
bamboo = mkN 0.36822465538989846 0.28753796455571745 -0.6751128795034731 0.2815561028310949 0.9426963580732695 -0.5627135006548016 0.6272896542738557 0.15961827052268163 0.9542361959129266 0.41881754146665173 -0.8423411307260231 -0.8567205257173853 -0.1115125968415871 0.5858225571930564 2.281560553806017e-3 2.632745342867839 -4.7696445416913615 9.480508640478066e-2 1.950841592968882 7.441447122754341e-2 0.2925467513340605 -0.8075897907332612 -3.5596311909815217 -1.3902597787890736 -0.23485494782089522 -0.7877261530233384 -0.47170917113706146 -0.8734861984778206 0.7416573315171027 -1.6035820381023953 3.2747112499825546 1.9267604980849655 0.384964602027568 0.609967218519331 -5.624793301537245 0.9011691540606219 -1.512942948660343 -0.22776856710884685 0.6443840546096558 -9.16497472744208e-2 8.325172893764389 -3.402481463522075 -0.9978323040631123 -0.7355185966791216 1.2819102250407064 -1.3580398642663654 1.376839970026857 -4.439588669211025 12.339379979698098 -0.9446159201010953 -0.11896747100265784 -1.3590199539634278 -0.44416271202537927 6.928008462791044e-2 -1.7702186088964744 -2.934114582008699 2.7738147195530027 -1.1462410298296284 -0.6944171200058937 2.3460093617924276 -5.365678828677909 0.9428038696042964 -2.5613375209303433 0.5435611097599122 0.8177526289554715 -0.6412468072034783 1.0359195703531547 -2.2900630065831953 -3.624683808495947e-2 -1.1786373305521693 2.5071806845523987 -0.30109174883230416 -2.26693588706698 1.3734007078920447 0.4331461461699744 5.03295915276942 -0.4652560407014895


billBorr :: _ => Weights 41 _
billBorr = mkN -0.48965642045315505 -6.498321089168185 -4.4148344756894886e-2 5.402637293353525 0.26863568252449355 1.4053750386189998 3.435554461504253e-2 4.095377999304969 3.2895472219893556 -4.938601654734194 3.5740058730567537 -6.276738215801326 -2.0189671294852403 -0.2632773194917988 -8.682501286437642 0.2650842975879531 -4.320489407385427 -0.5759683988082047 7.067044207732176 1.452935005087964 -1.889488367940781e-2 -6.751217326288785 4.246780318161066e-3 -4.137532690491444 2.933074556484148 4.568643354899606 -4.586784217888594e-2 -0.8272451810195688 3.388518222993371 -2.307623818204077 3.5574872771339954 -1.4024464542397694 -6.333555726145837 -0.26626607668403435 2.430250463381924 9.193608295010538 -1.020281417914501 6.0969909979778745 4.51264832501816 -2.7712656856484132 4.818250565952107


