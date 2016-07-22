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


type Distp = Int
type Situation = [(Int, Prob)]
type Prob = Double
type Goal = [Int]


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


update :: [Bool] -> (State Situation ())
update bs = do
        n <- get
        put $ newRound bs n


---- Mode ----



---- facility ----
mapSnd :: (a -> b) -> [(c, a)] -> [(c, b)]
mapSnd f xs = map (fSnd f) xs

fSnd :: (a -> b) -> (c, a) -> (c, b)
fSnd f (x, y) = (x, f y)
----- Test -------

-- _ = runState (update [True, True, True] ) testSituation


testSituation :: Situation
testSituation = [(1, 1.0), (2, 1.0), (3, 1.0)]


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




main :: IO ()
main = do
    let initial = [(1, 1/6), (2, 1/6), (3, 1/6), (4, 1/6), (5, 1/6), (6, 1/6)]
    onGoing $ return initial
    return ()



getDiceMaybe :: IO [Int]
getDiceMaybe = do
    input <- getLine
    case (readMaybe input :: Maybe [Int]) of
        Nothing -> do
            case isGoal input of
                False -> do
                    print ("wrong input")
                    getDiceMaybe
                True
        Just as -> if maximum as <= round(sideOfDice) then return as
            else
                do
                print ("wrong size number")
                getDiceMaybe





onGoing :: IO (Situation) -> IO (Situation)
onGoing s = do
    state <- s
    pars <- getDiceMaybe
    inputB <- trans pars
    let output = runState (update inputB ) state
    print $ snd output
    onGoing $ return $ snd output


--------------------------------
-- prNext_single ::
-- prNext :: Record -> Prob
-- prNext r = map snd

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