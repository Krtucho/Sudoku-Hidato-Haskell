module Main where
import Lib
import System.IO
-- import PdePreludat

import Data.List

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set, lookupMin, lookupMax)
import qualified Data.Set as Set

-- import Control.Lens

import System.Random
import System.IO.Unsafe

main :: IO ()
main = do putStr "What is your first name? "

some :: String -> IO ()
some a = do putStrLn (a ++ "\n" ++ "aaaaa")
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


-------------------------------------------------------GENERADOR------------------------------------------------------------------------------------

createMatrix::Int->Int->Matrix   --se le pasan la cantidad de filas y de columnas y se genera una matrix con esas dimensiones y 0 en todas las posiciones
createMatrix rows cols = transformMatrix [[0 | x <- [1..cols]] | y <- [1..rows]]

getFirstPosition::Int->Int->(Int,Int)--devuelve una fila y columna random dentro de los limites de la matrix
getFirstPosition rows cols = (getRandomNumber 0 (rows-1) , getRandomNumber 0 (cols-1))

getNumbersBoxes :: Matrix ->Int-> [Box] --se obtienen todos los boxes que tengan numeros distintos de -1, -1 y el maximo 
getNumbersBoxes m total = (Set.toList $ Set.filter (\box -> (value box /= -1 && value box /= 1 && value box /= total)) $ matrix m)

putEmptySpaces::Matrix->Int-> Matrix    --convertir todas las posiciones que tienen numeros diferentes del primero y el ultimo en 0 dejando los obstaculos
putEmptySpaces m total = addManyBoxes [ (x,y,0) | (Box x y _ ) <- (getNumbersBoxes m total)] m

putObstInEmptySpaces::Matrix-> Matrix    --convertir todas las posiciones que quedaron vacias en obstaculos
putObstInEmptySpaces m = addManyBoxes [ (x,y,-1) | (Box x y _ ) <- (getEmptyBoxes m)] m

getBoxFilCol :: Matrix ->Int->Int-> [Box]----------------se obtiene una lista con un solo box que tiene la fila columna y valor de una matrix segun la fila y columna que se pase
getBoxFilCol m r c= (Set.toList $ Set.filter (\box -> row box == r && col box == c) $ matrix m)

isUnique::Matrix->(Int,Int)->Int->Bool  --verifica que el hidato tenga solucion unica a partir del solucionador
isUnique m (x,y) total = length (solve m 2 (Box x y 1) (makeRestrictions m) total) == 1

--funcion que se le pasa una lista con todas las posibles posiciones en las que se puede poner un numero y devuelve una lista con las que mas soluciones descartan 

addValues::Matrix->Matrix->Int->Matrix  --se le pasa la matrix solucion, la plantilla que se tiene hasta el momento, la cantidad de cuadraditos a poner y se devuelve la matriz plantilla con estos puestos
addValues _ mTemp 0 = mTemp
addValues mFull mTemp count = let empties = getEmptyBoxes mTemp --lista de las posiciones vacias en temp
                                  rand = getRandomNumber 0 (length empties -1) --tomo un random entre 0 y la cantidad de casillas que puedo rellenar
                                  (Box x y _ ) = empties !! rand --obtengo la fila y la columna indexando con el random en la lista de casillas que puedo rellenar
                                  (Box _ _ value ) = (getBoxFilCol mFull x y)!!0 --obtengo el valor que tiene la casilla seleccionada en la matrix llena
                                  mTempNew = addBox x y value mTemp --agrego el valor obtenido a la plantilla que estoy creando-
                              in addValues mFull mTempNew (count-1)

makeUniqueHidato:: Matrix -> Matrix -> (Int,Int) ->Int-> Matrix  --se va creando una plantilla de hidato con solucion unica
makeUniqueHidato mFull mTemp firstp total | isUnique mTemp firstp total = mTemp --si ya es unica esta solucion se devuelve
                                | otherwise = makeUniqueHidato mFull (addValues mFull mTemp (((length(getEmptyBoxes mTemp)) `div` 10) + 1)) firstp total --en caso de no ser unica se agrega un 5% casillas que quedan vacias y se suma 1 para que en casos pequennos sea 1 lo que se annada

getUniqueSolve:: Matrix -> (Int,Int) ->Int-> Matrix  --devuelve una plantilla de hidato correcta a partir de una solucion
getUniqueSolve mFull firstp total = let m = putEmptySpaces mFull total
                                        mTemp = addValues mFull m (total `div` 3) --annade el 33% de casillas, como restricciones, del total
                                    in makeUniqueHidato mFull mTemp firstp total

generate::Int->Int->Matrix --se le pasa la cantidad de filas y de columnas  
generate rows cols = let m = createMatrix rows cols  --creo la matriz como lista de listas
                         (x,y) = getFirstPosition rows cols  --obtener la posicion del numero 1
                         obst = (rows*cols)`div`3  --se crean un 33% de obstaculos
                         m3 = addBox x y 1 m     --agrego el 1 en la posicion seleccionada anteriormente
                         total = rows*cols-obst   --las casillas a llenar son la cantidad de casillas menos la cantidad de obstaculos
                         rest = makeRestrictions m3   --se agrega el 1 a las restricciones, las restricciones son las casillas que ya tenian numero puesto
                         firstSolution = head (solve m3 2 (Box x y 1) rest total)  --a partir de la primera matriz de soluciones del hidato mando a obtener una plantilla
                         matrix = putObstInEmptySpaces firstSolution  
                     in getUniqueSolve matrix (x,y) total-- que tenga solucion unica para devolverla directamente desde aqui

                                        
-------------------------------------------------------GENERADOR-FIN--------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------



--------------------------------------------------------PROBAR-PARTES-DEL-GENERADOR-------------------------------------------------------------------------------------------------


generateTemp::Int->Int->Matrix --se le pasa la cantidad de filas y de columnas  
generateTemp rows cols = let m = createMatrix rows cols  --creo la matriz como lista de listas
                             (x,y) = getFirstPosition rows cols  --obtener la posicion del numero 1
                             obst = (rows*cols)`div`4  --se crean un 20% de obstaculos
                             m3 = addBox x y 1 m     --agrego el 1 en la posicion seleccionada anteriormente
                             total = rows*cols-obst   --las casillas a llenar son la cantidad de casillas menos la cantidad de obstaculos
                             rest = makeRestrictions m3   --se agrega el 1 a las restricciones, las restricciones son las casillas que ya tenian numero puesto
                             firstSolution = head (solve m3 2 (Box x y 1) rest total)  --a partir de la primera matriz de soluciones del hidato mando a obtener una plantilla
                             matrix = putObstInEmptySpaces firstSolution  
                         in  matrix --(x,y) total-- que tenga solucion unica para devolverla directamente desde aqui

putEmptySpaces1::Matrix->Int-> Matrix    --convertir todas las posiciones que tienen numeros diferentes del primero y el ultimo en 0 dejando los obstaculos
putEmptySpaces1 m total = addManyBoxes [ (x,y,0) | (Box x y _ ) <- (getNumbersBoxes m total)] m

addValues1::Matrix->Matrix->Int->Matrix  --se le pasa la matrix solucion, la plantilla que se tiene hasta el momento, la cantidad de cuadraditos a poner y se devuelve la matriz plantilla con estos puestos
addValues1 _ mTemp 0 = mTemp
addValues1 mFull mTemp count = let empties = getEmptyBoxes mTemp --lista de las posiciones vacias en temp
                                   rand = getRandomNumber 0 (length empties -1) --tomo un random entre 0 y la cantidad de casillas que puedo rellenar
                                   (Box x y _ ) = empties !! rand --obtengo la fila y la columna indexando con el random en la lista de casillas que puedo rellenar
                                   (Box _ _ value ) = (getBoxFilCol mFull x y)!!0 --obtengo el valor que tiene la casilla seleccionada en la matrix llena
                                   mTempNew = addBox x y value mTemp --agrego el valor obtenido a la plantilla que estoy creando
                               in addValues mFull mTempNew (count-1)

--Tipo debuggeo
--mTemp = putEmptySpaces mFull total
--mTemp = addValues mFull m (total `div` 2) --annade la mitad de casillas, como restricciones, del total
--mTemp= addValues mFull m (((length(getEmptyBoxes m)) `div` 20) + 1)
--isUnique mTemp frts total
--makeUniqueHidato mFull mTemp frst total
---------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------

ma :: [[Int]]
ma = [      [-1, 5, 0, -1],
            [-1,0, 3, -1],
            [-1, 1, 0, -1]
            ]

-- printMatrix [a] = putStrLn a
-- printMatrix (x:xs) = do putStrLn x
--                         printMatrix xs

-- maxr = 2
-- maxc = 3

dir = [(-1,-1),(-1,0),(-1,1),(0,1),(1,1),(1,0),(1,-1),(0,-1)]

findAdjacents (r,c)= [(r + fst x, c + snd x) | x <- dir]

-- validPos (x,y) = (x >= 0) && (y >= 0) && (x <= maxr) && (y <= maxc) && matrix !! x !! y >= 0

isAdjacent :: Box -> Box -> Bool
isAdjacent (Box r1 c1 _) (Box r2 c2 _) = abs (r1 - r2) < 2 && abs (c1 - c2) < 2

nextStep :: Matrix -> Int -> Box -> [Box] -> Map Int Box -> [(Matrix, Box)]
nextStep m step prevPos adjacents restrictions |Map.member step restrictions && isAdjacent (restrictions Map.! step) prevPos = [(m, restrictions Map.! step)]
                                  | Map.member step restrictions && not (isAdjacent (restrictions Map.! step) prevPos) = []--findValue step m && not valueIsOk step m = []
                                  | otherwise = [(addBox (row x) (col x) (step) m, x)| x <- unorderList adjacents, canSetInAdj (row x, col x) m]
-- total = 5
solve :: Matrix -> Int -> Box -> Map Int Box -> Int -> [Matrix]
solve m step pos restrictions total | step == total + 1 = [m]
                            --  | step == total = m
                            --  |  step == total = m
                              | otherwise = let xs = nextStep m step pos (getAdj ((row pos), (col pos), (value pos)) (rows m, cols m)) restrictions in concat [solve matrix (step+1) box restrictions total| (matrix, box) <- xs]

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
    rows :: Int,
    cols :: Int,
    matrix :: Set Box                    
}

instance Show Matrix where
    show m = "\n\t{\n \t" ++ intercalate "\n \t" (map (\x -> show x) (getRows maxr m)) ++ "\n\t\t}" 
                where maxr = row (getMax m)

-- temp = Matrix{
--     matrix = Set.singleton (Box {row = 0, col = 0, value = 0})
-- } 


-- addBox m val = Matrix { matrix = Set.insert (Box {row = 0, col = 1, value = val}) $ matrix m }



findBox x y m = Set.member Box{row=x, col=y, value=0} $ matrix m

addBox :: Int -> Int -> Int -> Matrix -> Matrix
addBox x y val m = Matrix { rows=rows m, cols=cols m, matrix = Set.insert (Box {row = x, col = y, value = val}) $ matrix m }

-- addB x y val m =  Set.insert (Box {row = x, col = y, value = val}) $ matrix m

addManyBoxes :: [(Int, Int, Int)] -> Matrix -> Matrix
addManyBoxes [] m = m
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

singletonMatrix rows cols = Matrix{
    rows=rows,
    cols=cols,
    matrix = Set.singleton (Box {row = 0, col = 0, value = 0})
} 

mapMatrix m maxr maxc  = [(x,y, m!!x!!y) | x <- [0..maxr-1], y <- [0..maxc-1]]

transformMatrix :: [[Int]] -> Matrix
transformMatrix m_in = addManyBoxes (mapMatrix m_in (length m_in) (length (m_in!!0))) (singletonMatrix (length m_in) (length (m_in!!0)))

-- takeAdj (r,c) m = Set.filter (\(x) -> col x == c && row x == r ) $ matrix m--findAdjacents (r,c)
getAdj :: (Int, Int, Int) -> (Int, Int) -> [Box]
getAdj (r,c, v) (maxr, maxc) = [Box nr nc v| adj <- findAdjacents (r,c), let (nr, nc) = adj, nr >= 0, nc >= 0, nr < maxr, nc < maxc]

canSetInAdj :: (Int,Int) -> Matrix -> Bool
canSetInAdj (r,c) m |let box = Set.elemAt (Set.findIndex (Box r c 0) (matrix m)) (matrix m) in (value box) == 0 = True
                    | otherwise = False

makeRestrictions :: Matrix -> Map Int Box
makeRestrictions m = Map.fromList (map (\x-> (value x, x)) (Set.toList $ Set.filter (\box -> value box > 0) $ matrix m))

-- restrictions = makeRestrictions temp

findBorders :: [[Int]] -> (Int, Int)
findBorders m = let maxr = length m
                in if maxr > 0 then (maxr, length (m!!0)) else (maxr, 0)


-- nextStep temp 2 (Box 2 1 1) (getAdj (2,1,2)) restrictions
solveHidato :: [[Int]] -> (Int, Int, Int) -> Int -> [Matrix]
solveHidato m pos total = let m_tr = transformMatrix m
                              restrictions = (makeRestrictions m_tr)
                       in solve m_tr 2 (Box x y z) restrictions total
                      where (maxr,maxc) = findBorders m
                            (x, y, z) = pos

-- shuffle' :: [Int] -> [a] -> [a]
-- shuffle' (i:is) xs = let (firsts, rest) = splitAt (i `mod` length xs) xs
--                      in (head rest) : shuffle' is (firsts ++ tail rest)

getRandomNumber :: Int -> Int -> Int
getRandomNumber a b = unsafePerformIO (randomRIO (a, b) :: IO Int)
-- num = unsafePerformIO (randomRIO (0, 10) :: IO Int)

replaceLst :: Int -> [a] -> [a]
replaceLst x xs = let (a,b) = splitAt x xs
                 in (if length b > 0 then [head b] else []) ++(if length a > 0 then tail a else []) ++ (if length a > 0 then [head a] else [])++(if length b > 0 then tail b else [])

unorderList :: [a] -> [a]
unorderList xs = shuffle' xs (len-1) (map (\x -> getRandomNumber 0 len) [1..len])
                    where len = length xs

shuffle' :: [a] -> Int -> [Int] -> [a] 
-- shuffle' xs 0 rpl = 
shuffle' xs index rpl | index == 0 = replaceLst (rpl!!0) xs
                      | otherwise = shuffle' (replaceLst (rpl!!index) xs) (index-1) rpl

getEmptyBoxes :: Matrix -> [Box]
getEmptyBoxes m = (Set.toList $ Set.filter (\box -> value box == 0) $ matrix m)

getObstacles :: Matrix -> [Box]
getObstacles m = (Set.toList $ Set.filter (\box -> value box == -1) $ matrix m)


getRow index m = Set.toList $ Set.filter (\x -> (row x) == index) $ m
printRow row = (map (\x -> value x) row)
getRows maxr m = map (\y -> printRow y) $ (map (\x -> getRow x (matrix m)) [0..maxr])
getMax m = let Just val = Set.lookupMax (matrix m)
            in val

getMatrixRow index m = m!!index
getMatrixRows maxr m = (map (\x -> m!!x) [0..maxr])

printMatrix :: [[Int]] -> IO ()
printMatrix m = do putStrLn ("\n\t{\n \t" ++ intercalate "\n \t" (map (\x -> show x) (getMatrixRows maxr m)) ++ "\n\t\t}")
                where maxr = (length m) - 1

-- random_obstaculos :: Int -> Int
-- random_obstaculos = getRandomNumber 1 3

-- loadHidato :: String -> [[Int]]
-- loadHidato file_path = do text <- readFile file_path 
--                           return read text::[[Int]]

-- Muestra los comandos disponibles
help :: IO()
help = do putStrLn "Comandos \n\t\tResolver un Hidato \n\tsolveHidato <hidato> <pos_inicial> <max_value>\n\n\t    <hidato>: debe ser una lista de listas que tenga este formato: [[Int]]\n\t    <pos_inicial>: debe de ser donde se encuentra el valor 1 en el Hidato con el siguiente formato (row,col,val), donde val tiene que ser igual a 1\n\t    <max_value>: Valor maximo que se encuentra en Hidato\n\tEjemplo:\n\tghci>m=[[0,0,4,0],[1,0,0,-1],[-1,0,0,9],[0,14,0,0]]\n\tghci>solveHidato m (1,0,1) 14\n\t"

-- :: String -> IO ()
-- some a = do putStrLn (a ++ "\n" ++ "aaaaa")

-- (4,6) (3,4) [[0,33,35,0,0,-1,-1,-1],[0,0,24,22,0,-1,-1,-1],[0,0,0,21,0,0,-1,-1],[0,26,0,13,40,11,-1,-1],[27,0,0,0,9,0,1,-1],[-1,-1,0,0,18,0,0,-1],[-1,-1,-1,-1,0,7,0,0],[-1,-1,-1,-1,-1,-1,5,0]]
--test 1
test1 :: [[Int]]
test1 = [[0,33,35,0,0,-1,-1,-1],[0,0,24,22,0,-1,-1,-1],[0,0,0,21,0,0,-1,-1],[0,26,0,13,40,11,-1,-1],[27,0,0,0,9,0,1,-1],[-1,-1,0,0,18,0,0,-1],[-1,-1,-1,-1,0,7,0,0],[-1,-1,-1,-1,-1,-1,5,0]]

-- (1,0) (3,1) [[0,0,4,0],[1,0,0,-1],[-1,0,0,9],[0,14,0,0]]
test2 :: [[Int]]
test2 = [[0,0,4,0],[1,0,0,-1],[-1,0,0,9],[0,14,0,0]]

-- (0,7) (5,0) [[-1,-1,-1,-1,-1,-1,0,1,-1,-1],[0,0,-1,-1,4,0,0,38,0,35],[73,0,75,0,0,41,0,0,34,0],[0,78,0,0,6,0,8,0,31,0],[0,0,68,0,0,0,44,0,0,32],[80,50,64,0,0,0,11,21,0,0],[0,51,49,0,47,0,0,20,23,0],[53,0,0,48,14,16,18,0,27,0],[55,0,0,58,0,0,-1,-1,25,0],[-1,-1,57,59,-1,-1,-1,-1,-1,-1]]
test3 :: [[Int]]
test3 = [[-1,-1,-1,-1,-1,-1,0,1,-1,-1],[0,0,-1,-1,4,0,0,38,0,35],[73,0,75,0,0,41,0,0,34,0],[0,78,0,0,6,0,8,0,31,0],[0,0,68,0,0,0,44,0,0,32],[80,50,64,0,0,0,11,21,0,0],[0,51,49,0,47,0,0,20,23,0],[53,0,0,48,14,16,18,0,27,0],[55,0,0,58,0,0,-1,-1,25,0],[-1,-1,57,59,-1,-1,-1,-1,-1,-1]]