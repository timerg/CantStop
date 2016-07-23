{-# Language NoMonomorphismRestriction, GADTs #-}
module Func where

import Lib.Parser
import System.Random
import Text.Read (readMaybe)
import Control.Monad.State

-- type State s = StateT s Identity

sideOfDice = 6

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in  (firstCoin, secondCoin, thirdCoin)

-- mkStdGen :: Int -> StdGen
-- threeCoins $ mkStdGen 3


type Prob = Double
type Situation = [(Int, Prob)]
type Goal = [Int]
data Status = St {
    goal :: Goal,
    situation :: Situation
} deriving(Show)
type GoalProbList = [(Int, Prob)]


--         round        probdistribution
-- type State a = StateT a [Prob]




nextProb :: Bool -> (Int, Prob) -> (Int, Prob)
nextProb b (i, p) = case b of
                        True -> (i, p * 1 / sideOfDice)
                        False -> (i, p * (sideOfDice - 1) / sideOfDice)

newRound :: [Bool] -> Situation -> Situation
newRound bs ss = normalize $ zipWith nextProb bs ss


normalize :: Situation -> Situation
normalize s = mapSnd ( / total) s
        where total = sum $ map snd s


updateS :: [Bool] -> (State Status ())
updateS bs = do
        n <- get
        let gol = goal n
            sit = situation n
        put (St gol (newRound bs sit))

updateGoal :: Int -> (State Status ())
updateGoal g = do
        n <- get
        let gol = goal n
            sit = situation n
        put (St (g:gol) sit)

updateGoals :: Goal -> (State Status ())
updateGoals gs = do
        n <- get
        let sit = situation n
        put (St gs sit)

---- Mode ----



---- Widget ----
mapSnd :: (a -> b) -> [(c, a)] -> [(c, b)]
mapSnd f xs = map (fSnd f) xs

fSnd :: (a -> b) -> (c, a) -> (c, b)
fSnd f (x, y) = (x, f y)

square a = a^2

data PI a = PI a (PI a)| One deriving (Show)

instance Functor (PI) where
    fmap f One = One
    fmap f (PI a ps) = PI (f a) (fmap f ps)

piSum :: PI Double -> Double
piSum One = 1
piSum (PI a (ps)) = a * (piSum ps)


----- Test -------

-- _ = runState (update [True, True, True] ) testSituation


-- testSituation :: Situation
-- testSituation = [(1, 1.0), (2, 1.0), (3, 1.0)]


----- Parser -----
replaceNth n newVal (x:xs)
    | n == 1 = newVal:xs
    | otherwise = x:replaceNth (n - 1) newVal xs


replaceMulti [] newVal xs = xs
replaceMulti (n:ns) newVal (x:xs)
    = replaceMulti ns newVal $ replaceNth n newVal (x:xs)

trans :: [Int] -> IO [Bool]
trans ns = do
    let initial = [False, False, False, False, False, False]
    return $ replaceMulti ns True initial





getGoal :: Status -> IO Status
getGoal s = do
    -- state <- s
    print("Add the new goal:")
    newGoal <- getLine
    case (readMaybe newGoal :: Maybe Int) of
        Nothing -> do
            print("The new goal is an Int")
            getGoal s
        Just i -> if elem i (goal s) == True
            then do print("This Goal exist")
                    return s
            else do
                    let output =  execState (updateGoal i) s
                    print $ "Now the goals are " ++ show(goal output)
                    return output



getDiceMaybe :: Status -> IO ([Int], Goal)
getDiceMaybe s = do
    input <- getLine
    if input == "goal"
        then do
            sNew <- getGoal s
            getDiceMaybe sNew
        else
            case (readMaybe input :: Maybe [Int]) of
                Nothing -> do
                    print ("wrong input")
                    getDiceMaybe s
                Just as -> if maximum as <= round(sideOfDice) then return (as, goal s)
                    else
                        do
                        print ("wrong size number")
                        getDiceMaybe s







onGoing :: Status -> IO (Status)
onGoing state = do
    pars <- getDiceMaybe state
    inputB <- trans (fst pars)
    let gol = snd pars
    let output = snd $ runState (updateGoals gol) $
            snd $ runState (updateS inputB ) state
    let sit = situation output
    print $ map (goalProb sit) gol
    onGoing $ output

main :: IO ()
main = do
    let initial = St [] [(1, 1/6), (2, 1/6), (3, 1/6), (4, 1/6), (5, 1/6), (6, 1/6)]
    onGoing initial
    return ()

--------------------------------


pb :: Situation -> Int -> Prob
pb [] i = error "wrong index"
pb (x:xs) i = if (fst x) == i then snd x
                else pb xs i

findGoalProb :: Status -> GoalProbList
findGoalProb s = zip gol $ map (goalProb sit) gol
    where   gol = goal s
            sit = situation s

goalProb :: Situation -> Int -> Prob
goalProb s i
    | i == 2 = piSum $ fmap (pb s) $ PI 1 $ PI 1 $ One
    | i == 3 = (*) 2 $ piSum $ fmap (pb s) $ PI 1 $ PI 2 $ One
    | i == 4 = (piSum $ fmap (pb s) $ PI 2 $ PI 2 $ One) +
               ((*) 2 $ piSum $ fmap (pb s) $ PI 1 $ PI 3 $ One)
    | i == 5 = ((*) 2 $ piSum $ fmap (pb s) $ PI 1 $ PI 4 $ One) +
               ((*) 2 $ piSum $ fmap (pb s) $ PI 2 $ PI 3 $ One)
    | i == 6 = (piSum $ fmap (pb s) $ PI 3 $ PI 3 $ One) +
               ((*) 2 $ piSum $ fmap (pb s) $ PI 2 $ PI 4 $ One) +
               ((*) 2 $ piSum $ fmap (pb s) $ PI 1 $ PI 5 $ One)
    | i == 7 = ((*) 2 $ piSum $ fmap (pb s) $ PI 1 $ PI 6 $ One) +
               ((*) 2 $ piSum $ fmap (pb s) $ PI 2 $ PI 5 $ One) +
               ((*) 2 $ piSum $ fmap (pb s) $ PI 3 $ PI 4 $ One)
    | i == 8 = (piSum $ fmap (pb s) $ PI 4 $ PI 4 $ One) +
               ((*) 2 $ piSum $ fmap (pb s) $ PI 3 $ PI 5 $ One) +
               ((*) 2 $ piSum $ fmap (pb s) $ PI 2 $ PI 7 $ One)
    | i == 9 = (piSum $ fmap (pb s) $ PI 3 $ PI 6 $ One) +
               ((*) 2 $ piSum $ fmap (pb s) $ PI 4 $ PI 5 $ One)
    | i == 10 = (piSum $ fmap (pb s) $ PI 5 $ PI 5 $ One) +
                ((*) 2 $ piSum $ fmap (pb s) $ PI 4 $ PI 6 $ One)
    | i == 11 = (*) 2 $ piSum $ fmap (pb s) $ PI 5 $ PI 6 $ One
    | i == 12 = piSum $ fmap (pb s) $ PI 6 $ PI 6 $ One

    --------------------------------
    -------------------------------
testSit = [(1, 1/6), (2, 1/6), (3, 1/6), (4, 1/6), (5, 1/6), (6, 1/6)]



-- Px for x = [1..6] means the chance to roll the side x
-- PSy for y = [2 ..36] means the chance for rolling value y
-- PS2  = P1 * P1
-- PS3  = P2 * P1 * 2
-- PS4  = P2 * P2  + P1 * P3 * 2
-- PS5  = P1 * P4 * 2  + P2 * P3 * 2
-- PS6  = P1 * P5 * 2  + P2 * P4 * 2  + P3 * P3
-- PS7  = P1 * P6 * 2  + P2 * P5 * 2  + P3 * P4 * 2
-- PS8  = P2 * P6 * 2  + P3 * P5 * 2  + P4 * P4
-- PS9  = P3 * P6 * 2  + P4 * P5 * 2
-- PS10 = P4 * P6 * 2  + P5 * P5
-- PS11 = P5 * P6 * 2
-- PS12 = P6 * P6



-- If the x side appear m times in n times rolling,
-- the chance for the next times it appears is:
-- Px = (5/6)^(n-m) * (1/6)^(m+1)






















--