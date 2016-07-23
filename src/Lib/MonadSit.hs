{-# Language NoMonomorphismRestriction, GADTs #-}
import Control.Monad.State

type Prob = Double
type Situation a = [(a, Prob)]


-- s should be the dice point(or side)

instance Functor Ms where
    fmap f (BoxS []) = BoxS []
    fmap f (BoxS s) = BoxS (mapFst f s)
            where mapFst f [] = []
                  mapFst f ((a, b):xs) = (f a, b) : (mapFst f xs)

instance Applicative Ms where
    pure a = BoxS [(a, 1/6)]
    f <*> s = do
        ms <- s
        mf <- f
        return $ mf ms

instance Monad Ms where
    return = pure
    xs >>= f = join $ fmap f xs


data Ms a where
    BoxS :: Situation a -> Ms a
    deriving (Show)

combine :: (Int, Prob) -> (Int, Prob) -> (Int, Prob)
combine (i1, p1) (i2, p2) = (i1 + i2, p1 * p2)

extent :: Int -> Ms Int -> Ms Int -> Ms Int
extent gol s1 s2 = do
    se1 <- s1
    se2 <- s2
    if (se1 + se2) == gol
        then return se1
        else return se2

--
-- isGoal :: Int -> (Int, Prob) -> Bool
-- isGoal = _


testMs1 = BoxS [(1, 1/6)]
testMs2 = BoxS [(2, 2/6), (3, 3/6)]










-- class Monad d => Dist d where
--   bern :: Prob -> d Bool              -- (a -> mb)  for the >>=
--   failure :: d a                      --
--
-- data Dst a where
--   Single :: PT a -> Dst a
--   Chain  :: PT b -> (b -> Dst a) -> Dst a
--
-- instance Dist Dst where
--   bern p  = Single [(True, p), (False,1-p)]
--   failure = Single []
--
-- girl :: Dst Bool
-- girl = do
--   g1 <- bern 0.5
--   g2 <- bern 0.5
--   if g1 || g2 then return g1
--       else failure
--
-- drunk_coin :: Dst Bool
-- drunk_coin = do
--   coin <- bern 0.5
--   lost <- bern 0.9
--   if lost then failure else return coin


