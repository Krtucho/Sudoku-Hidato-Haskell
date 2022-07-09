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

ma :: [[Int]]
ma = [      [-1, 5, 0],
            [-1,-1, 0],
            [-1, 1, 0]
            ]


-- (4,6) (3,4) 
mn :: [[Int]]
mn=  [  
       [0,33,35,0,0,-1,-1,-1],
       [0,0,24,22,0,-1,-1,-1],
       [0,0,0,21,0,0,-1,-1],
       [0,26,0,13,40,11,-1,-1],
       [27,0,0,0,9,0,1,-1],
       [-1,-1,0,0,18,0,0,-1],
       [-1,-1,-1,-1,0,7,0,0],
       [-1,-1,-1,-1,-1,-1,5,0]
       ]


-- printMatrix [a] = putStrLn a
-- printMatrix (x:xs) = do putStrLn x
--                         printMatrix xs

maxr = 2
maxc = 2

dir = [(-1,-1),(-1,0),(-1,1),(0,1),(1,1),(1,0),(1,-1),(0,-1)]

findAdjacents (r,c)= [(r + fst x, c + snd x) | x <- dir]

-- validPos (x,y) = (x >= 0) && (y >= 0) && (x <= maxr) && (y <= maxc) && matrix !! x !! y >= 0

isAdjacent :: Box -> Box -> Bool
isAdjacent (Box r1 c1 _) (Box r2 c2 _) = abs (r1 - r2) < 2 && abs (c1 - c2) < 2

nextStep :: Matrix -> Int -> Box -> [Box] -> Map Int Box -> [(Matrix, Box)]
nextStep m step prevPos adjacents restrictions |Map.member step restrictions && isAdjacent (restrictions Map.! step) prevPos = [(m, restrictions Map.! step)]
                                  | Map.member step restrictions && not (isAdjacent (restrictions Map.! step) prevPos) = []--findValue step m && not valueIsOk step m = []
                                  | otherwise = [(addBox (row x) (col x) (step) m, x)| x <- adjacents, canSetInAdj (row x, col x) m]
total = 5
solve :: Matrix -> Int -> Box -> Map Int Box -> [Matrix]
solve m step pos restrictions | step == total + 1 = [m]
                            --  | step == total = m
                            --  |  step == total = m
                              | otherwise = let xs = nextStep m step pos (getAdj ((row pos), (col pos), (value pos))) restrictions in concat [solve matrix (step+1) box restrictions | (matrix, box) <- xs]

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

addBox :: Int -> Int -> Int -> Matrix -> Matrix
addBox x y val m = Matrix { matrix = Set.insert (Box {row = x, col = y, value = val}) $ matrix m }

-- addB x y val m =  Set.insert (Box {row = x, col = y, value = val}) $ matrix m

addManyBoxes :: [(Int, Int, Int)] -> Matrix -> Matrix
addManyBoxes [x] m = let (r,c,val) = x in addBox r c val m
addManyBoxes (x:xs) m = let (r,c,val) = x in addManyBoxes xs (addBox r c val m) 

-- xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]

-- listTraversal (x:xs) = zip xxs [0..2]-- [ zip(x,y) | x <- xs, y<- [1..3]]
-- -- listValues (x:xs)
-- -- convertMatrix m = 

-- -- dfs (x, y) (x, y) h = (True, h)
-- test [x] = let (a,b,c) = x
--                 in show (a,b,c)
-- test (x:xs) = let (a,b,c) = x
--                 in show (a,b,c) ++ test xs

test2 m maxr maxc  = [(x,y, m!!x!!y) | x <- [0..maxr], y <- [0..maxc]]

transformMatrix :: [[Int]] -> Int -> Int -> Matrix
transformMatrix m_in maxr maxc = addManyBoxes (test2 m_in maxr maxc) temp

-- takeAdj (r,c) m = Set.filter (\(x) -> col x == c && row x == r ) $ matrix m--findAdjacents (r,c)
getAdj :: (Int, Int, Int) -> [Box]
getAdj (r,c, v) = [Box nr nc v| adj <- findAdjacents (r,c), let (nr, nc) = adj, nr >= 0, nc >= 0, nr <= maxr, nc <= maxc]

canSetInAdj (r,c) m |let box = Set.elemAt (Set.findIndex (Box r c 0) (matrix m)) (matrix m) in (value box) == 0 = True
                    | otherwise = False

makeRestrictions :: Matrix -> Map Int Box
makeRestrictions m = Map.fromList (map (\x-> (value x, x)) (Set.toList $ Set.filter (\box -> value box > 0) $ matrix m))

restrictions = makeRestrictions temp

-- nextStep temp 2 (Box 2 1 1) (getAdj (2,1,2)) restrictions
