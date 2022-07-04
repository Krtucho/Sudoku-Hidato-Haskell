module Main where
import Lib
import System.IO
-- import PdePreludat

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set, lookupMin, lookupMax)
import qualified Data.Set as Set

-- import Control.Lens

main :: IO ()
main = do putStr "What is your first name? "
                -- first <- getLine
                -- putStr "And your last name? "
-- foo :: Maybe String
-- foo = do
--         x <- Just 3
--         y <- Just "!"
--         Just (show x ++ y)

-- someFunc

-- matrix = [  [-1, 5, 4],
--             [-1,-1, 3],
--             [-1, 1, 2]
--             ]

ma = [  [-1, 5, 0],
            [-1,-1, 0],
            [-1, 1, 0]
            ]

-- printMatrix [a] = putStrLn a
-- printMatrix (x:xs) = do putStrLn x
--                         printMatrix xs

maxr = 2
maxc = 2

dir = [(-1,-1),(-1,0),(-1,1),(0,1),(1,1),(1,0),(1,-1),(0,-1)]

findAdjacents (r,c)= [(r + fst x, c + snd x) | x <- dir]

-- validPos (x,y) = (x >= 0) && (y >= 0) && (x <= maxr) && (y <= maxc) && matrix !! x !! y >= 0

-- nextStep m step prevPos adjacents | findValue step m && valueIsOk step m = m
--                                   | findValue step m && not valueIsOk step m = []
--                                   | otherwise = [x | x <- adjacents, canMoveToPos x]
total = 5
solve m step pos | step == total + 1 = m
                --  | step == total = m
                --  |  step == total = m
                 |  otherwise = let xs = []-- nextStep ...
                                in [x | x <- xs]

data Box = Box {
    row::Int,
    col::Int,
    value:: Int
}

instance Show Box where
    show box = "(" ++ show (row box) ++ "," ++ show (col box) ++ "," ++ show (value box) ++ ")"

instance Eq Box where {
    b1 == b2 = row b1 == row b2 && col b1 == col b2
}

instance Ord Box where
    compare b1 b2
            | row b1 /= row b2 = compare (row b1) (row b2)
            | row b1 == row b2 = compare (col b1) (col b2)
            | col b1 == col b2 = EQ

data Matrix = Matrix { 
    matrix :: Set Box
                    
} deriving(Show)


temp = Matrix{
    matrix = Set.singleton (Box {row = 0, col = 0, value = 0})
} 


-- addBox m val = Matrix { matrix = Set.insert (Box {row = 0, col = 1, value = val}) $ matrix m }



findBox x y m = Set.member Box{row=x, col=y, value=0} $ matrix m


addBox x y val m = Matrix { matrix = Set.insert (Box {row = x, col = y, value = val}) $ matrix m }

addB x y val m =  Set.insert (Box {row = x, col = y, value = val}) $ matrix m

addManyBoxes [(x, y, val)] m = addB (x y val) m
addManyBoxes (x:xs) m = let (x,y,val) =  addB (x y val) m $ addManyBoxes xs m

xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]

listTraversal (x:xs) = zip xxs [0..2]-- [ zip(x,y) | x <- xs, y<- [1..3]]
-- listValues (x:xs)
-- convertMatrix m = 

-- dfs (x, y) (x, y) h = (True, h)
test [x] = let (a,b,c) = x
                in show (a,b,c)
test (x:xs) = let (a,b,c) = x
                in show (a,b,c) ++ test xs

test2 m = [(x,y, m!!x!!y) | x <- [0..1], y <- [0..1]]