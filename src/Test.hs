{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
module Test(Test, testSimulatorInstance, testNeuralInstance) where

import Brain
import Convenience
import Minimizer
import Simulator
import Graphics.Gloss

type Updater = [Vec Double] -> Vec Double -> [Vec Double]

newtype Test = Test ([Vec Double],(Vec Double, Double),Updater)

testSimulatorInstance :: Simulator Test
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

        mainState = randTests (100,100) (applyBeforeBox bronx neuralUpdater) 114678

computeGoal ::(Vec Double, Double) -> Vec Double
computeGoal (loc, angle) = rotateVec (fromScalar 100) angle + loc

randTests :: (Double,Double) -> Updater -> Double -> Test
randTests numTestsRange updater seed = Test (points, (randGoal,0), updater)
    where range = (-500,500)
          randGoal = Vec (pseudoRand range seed, pseudoRand range (seed+1))
          numTests = pseudoRand numTestsRange (seed+2) & floor
          points = zipWith (curry Vec)
                           (pseudoRands range (seed+3))
                           (pseudoRands range (seed+4))
                   & take numTests

myUpdater points goal = points &> (\p -> (goal - p)/(dist goal p & fromScalar))

neuralUpdater :: Brain 4 2 w -> Weights w -> Updater
neuralUpdater (Brain feed) weights points goal =
    points &> (\p -> vecToSized (p,goal) & feed weights & sizedToVec)
  where
    vecToSized :: (Vec Double,Vec Double) -> Weights 4
    vecToSized (Vec (a,b), Vec (c,d)) = Sized [a,b,c,d]
    sizedToVec :: Weights 2 -> Vec Double
    sizedToVec (Sized [a,b]) = Vec (lerp -10 10 a, lerp -10 10 b)

testNeuralInstance :: NeuralSim Test _ _ _ _
testNeuralInstance = NeuralSim auto testSimulatorInstance currentBox randTrainingState neuralStep
    where
        currentBox@(brain,_,_) = bronx
        neuralStep (Test (a,b,_)) weights = _simStep testSimulatorInstance (Test (a,b,neuralUpdater brain weights))
        randTrainingState seed weights =
            randTests (10,10) (neuralUpdater brain weights) (seed+1)

box :: _ => BrainBox _ _ _ _
box = buildBrain (initBrain bamboo #> (Disable (biased @3)) #> biased @10 #> biased @2)


bamboo :: _ => Weights _
bamboo = mkN 0.36822465538989846 0.28753796455571745 -0.6751128795034731 0.2815561028310949 0.9426963580732695 -0.5627135006548016 0.6272896542738557 0.15961827052268163 0.9542361959129266 0.41881754146665173 -0.8423411307260231 -0.8567205257173853 -0.1115125968415871 0.5858225571930564 2.281560553806017e-3 2.632745342867839 -4.7696445416913615 9.480508640478066e-2 1.950841592968882 7.441447122754341e-2 0.2925467513340605 -0.8075897907332612 -3.5596311909815217 -1.3902597787890736 -0.23485494782089522 -0.7877261530233384 -0.47170917113706146 -0.8734861984778206 0.7416573315171027 -1.6035820381023953 3.2747112499825546 1.9267604980849655 0.384964602027568 0.609967218519331 -5.624793301537245 0.9011691540606219 -1.512942948660343 -0.22776856710884685 0.6443840546096558 -9.16497472744208e-2 8.325172893764389 -3.402481463522075 -0.9978323040631123 -0.7355185966791216 1.2819102250407064 -1.3580398642663654 1.376839970026857 -4.439588669211025 12.339379979698098 -0.9446159201010953 -0.11896747100265784 -1.3590199539634278 -0.44416271202537927 6.928008462791044e-2 -1.7702186088964744 -2.934114582008699 2.7738147195530027 -1.1462410298296284 -0.6944171200058937 2.3460093617924276 -5.365678828677909 0.9428038696042964 -2.5613375209303433 0.5435611097599122 0.8177526289554715 -0.6412468072034783 1.0359195703531547 -2.2900630065831953 -3.624683808495947e-2 -1.1786373305521693 2.5071806845523987 -0.30109174883230416 -2.26693588706698 1.3734007078920447 0.4331461461699744 5.03295915276942 -0.4652560407014895

bronx :: _ => BrainBox _ _ _ _
bronx = buildBrain (initBrain billBorr #> biased @3 #> biased @5 #> biased @2)

billBorr :: _ => Weights _
billBorr = mkN 5.938841861410319 -3.031344974028376 1.4079127620405796 3.0484355565437964 -1.3955430000091726 2.1501012545766427 4.157573482808591 0.5028923359874324 -4.1360094189758465 -0.481393147905702 3.014837376120888 -0.4417345846410652 4.914798204618693 0.4952873062093379 -4.920435353194186 -6.374535546813101e-2 3.4386959705820366 -5.571524292074402 -3.4553511783601807 -2.070242713924963 -2.932749561767211 5.941746590342621 4.90196848032054 4.829302524544937 2.240599385080958e-2 1.2787610225549386 -11.371035253748039 3.3399098262785025 -0.2120313358396704 -5.366867370363223 -18.91494528583692 -0.8055272627188008 -13.906080387627638 11.828186401218511 7.2054754905790155 3.9717913737458135 2.2300620441963246 -3.032449432979826 -1.5516822516194178 1.2047248917153295 -1.644926324861141 -3.346664644056715 3.936390226313713 -2.707904485853639 9.992496925922396 2.6090865080168673 -0.9454063498004938


