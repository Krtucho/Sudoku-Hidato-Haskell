module Main where

import System.IO
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set, lookupMin, lookupMax)
import qualified Data.Set as Set
import System.Random
import System.IO.Unsafe

main :: IO ()
main = do putStrLn "Welcome to our Sudoku-Hidato solver and generator....Type help :) \n"

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

------------------------------------------------------------------------- Data Structures ----------------------------------------------------------------------

--Box
data Box = Box {
    row::Int,
    col::Int,
    value:: Int
}
-- Redefiniendo el Show
instance Show Box where
    show box = "(" ++ show (row box) ++ "," ++ show (col box) ++ "," ++ show (value box) ++ ")"
-- Permitiendo que pertenezca a Eq, comparamos por las filas y las columnas de un Box
instance Eq Box where {
    b1 == b2 = row b1 == row b2 && col b1 == col b2
}
-- Permitiendo que pertenezca a Ord, comparamos por las filas y las columnas de un Box, el valor en si no nos interesa
instance Ord Box where
    compare b1 b2
            | row b1 /= row b2 = compare (row b1) (row b2)
            | row b1 == row b2 = compare (col b1) (col b2)
            | col b1 == col b2 = EQ

-- Matrix
data Matrix = Matrix {
    rows :: Int,
    cols :: Int,
    matrix :: Set Box                    
}
-- Redefiniendo el Show
instance Show Matrix where
    show m = "\n\t{\n \t" ++ intercalate "\n \t" (map (\x -> show x) (getRows maxr m)) ++ "\n\t\t}" 
                where maxr = row (getMax m)

-- Crea una matriz que tendra la cantidad rows de filas y cols de columnas
-- En un inicio tendra solamente 1 elemento (0,0,0) pero se utiliza luego para convertir una lista de listas [[Int]] en una Matrix
singletonMatrix rows cols = Matrix{
    rows=rows,
    cols=cols,
    matrix = Set.singleton (Box {row = 0, col = 0, value = 0})
} 

------------------------------------------------------------------ Data Structures ---------------------------------------------------------------

------------------------------------------------------------------ Data Structures Utils ---------------------------------------------------------
-- Agrega un elemento a la matrix de una Matrix. Se le pasan 4 valores, x=row y=col val=value y m = Matrix
addBox :: Int -> Int -> Int -> Matrix -> Matrix
addBox x y val m = Matrix { rows=rows m, cols=cols m, matrix = Set.insert (Box {row = x, col = y, value = val}) $ matrix m }

-- Dada una lista con un mapeo de una lista de listas [[Int]] y una matriz. Devuelve una matriz con todas esas posiciones
addManyBoxes :: [(Int, Int, Int)] -> Matrix -> Matrix
addManyBoxes [] m = m
addManyBoxes [x] m = let (r,c,val) = x in addBox r c val m
addManyBoxes (x:xs) m = let (r,c,val) = x in addManyBoxes xs (addBox r c val m) 

-- Dada lista de listas de enteros ([[Int]]) y unos limites de maxima cantidad de filas y maxima cantidad de columnas Devuelve una lista con una tupla de tres valores (row, col, val)
mapMatrix :: [[Int]] -> Int -> Int -> [(Int, Int, Int)]
mapMatrix m maxr maxc  = [(x,y, m!!x!!y) | x <- [0..maxr-1], y <- [0..maxc-1]]

-- Dada una lista de listas devuelve una Matrix mapeando sus valores a tipo Box
transformMatrix :: [[Int]] -> Matrix
transformMatrix m_in = addManyBoxes (mapMatrix m_in (length m_in) (length (m_in!!0))) (singletonMatrix (length m_in) (length (m_in!!0)))

--Valores a sumar en la fila y en la columna para las posibles direcciones que tomaremos al buscar los adyacentes
dir = [(-1,-1),(-1,0),(-1,1),(0,1),(1,1),(1,0),(1,-1),(0,-1)]

-- Dada una posicion devuelve una lista con los valores a sumar para obtener sus adyacentes
findAdjacents :: (Int, Int) -> [(Int, Int)]
findAdjacents (r,c)= [(r + fst x, c + snd x) | x <- dir]

-- Dada una posicion con un valor y los limites de una matriz devuelve una lista con posiciones adyacentes y el valor que se le paso
-- Lista de tipo Box, es decir Box row col value
getAdj :: (Int, Int, Int) -> (Int, Int) -> [Box]
getAdj (r,c, v) (maxr, maxc) = [Box nr nc v| adj <- findAdjacents (r,c), let (nr, nc) = adj, nr >= 0, nc >= 0, nr < maxr, nc < maxc]

-- Dada una posicion en una tupla (r,c) y una Matrix devuelve True si esa posicion se encuentra vacia
canSetInAdj :: (Int,Int) -> Matrix -> Bool
canSetInAdj (r,c) m |let box = Set.elemAt (Set.findIndex (Box r c 0) (matrix m)) (matrix m) in (value box) == 0 = True
                    | otherwise = False
-- Dados 2 elementos de tipo Box, verifica si ambos son adyacentes
isAdjacent :: Box -> Box -> Bool
isAdjacent (Box r1 c1 _) (Box r2 c2 _) = abs (r1 - r2) < 2 && abs (c1 - c2) < 2

-- Dada una Matrix devuelve un diccionario de (Int,Box) con todas las posiciones de la matriz que tengan valores mayores que 0
makeRestrictions :: Matrix -> Map Int Box
makeRestrictions m = Map.fromList (map (\x-> (value x, x)) (Set.toList $ Set.filter (\box -> value box > 0) $ matrix m))

-- Dada una lista de listas [[Int]] devuelve una tupla con los valores maximos de su fila y columna
findBorders :: [[Int]] -> (Int, Int)
findBorders m = let maxr = length m
                in if maxr > 0 then (maxr, length (m!!0)) else (maxr, 0)

-- Dada una matriz devuelve una lista de Box con todos sus casillas vacias (casillas que tengan valor 0)
getEmptyBoxes :: Matrix -> [Box]
getEmptyBoxes m = (Set.toList $ Set.filter (\box -> value box == 0) $ matrix m)

-- Dada una matriz devuelve una lista de Box con todos sus obstaculos (casillas que tengan valor -1)
getObstacles :: Matrix -> [Box]
getObstacles m = (Set.toList $ Set.filter (\box -> value box == -1) $ matrix m)

------------------------------------------------------------------ Data Structures Utils ---------------------------------------------------------

------------------------------------------------------------------ Solucionador ------------------------------------------------------------------
nextStep :: Matrix -> Int -> Box -> [Box] -> Map Int Box -> [(Matrix, Box)]
nextStep m step prevPos adjacents restrictions |Map.member step restrictions && isAdjacent (restrictions Map.! step) prevPos = [(m, restrictions Map.! step)]
                                  | Map.member step restrictions && not (isAdjacent (restrictions Map.! step) prevPos) = []--findValue step m && not valueIsOk step m = []
                                  | otherwise = [(addBox (row x) (col x) (step) m, x)| x <- unorderList adjacents, canSetInAdj (row x, col x) m]
-- total = 5
solve :: Matrix -> Int -> Box -> Map Int Box -> Int -> [Matrix]
solve m step pos restrictions total | step == total + 1 = [m]
                              | otherwise = let xs = nextStep m step pos (getAdj ((row pos), (col pos), (value pos)) (rows m, cols m)) restrictions in concat [solve matrix (step+1) box restrictions total| (matrix, box) <- xs]

-- Metodo principal para resolver un Hidato
-- Dada una lista de listas de enteros ([[Int]]), una posicion inicial y un valor maximo que contendra el Hidato lo soluciona y devuelve todas las soluciones posibles que encuentre 
solveHidato :: [[Int]] -> (Int, Int, Int) -> Int -> [Matrix]
solveHidato m pos total = let m_tr = transformMatrix m
                              restrictions = (makeRestrictions m_tr)
                       in solve m_tr 2 (Box x y z) restrictions total
                      where (maxr,maxc) = findBorders m
                            (x, y, z) = pos
------------------------------------------------------------------ Solucionador ------------------------------------------------------------------

------------------------------------------------------------------- Utils ------------------------------------------------------------------------

-- Dados 2 valores a y b devuelve un numero aleatorio entre los mismos 
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
shuffle' xs index rpl | index == 0 = replaceLst (rpl!!0) xs
                      | otherwise = shuffle' (replaceLst (rpl!!index) xs) (index-1) rpl

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

------------------------------------------------------------------- Utils ------------------------------------------------------------------------

-- loadHidato :: String -> [[Int]]
-- loadHidato file_path = do text <- readFile file_path 
--                           return read text::[[Int]]

-- Muestra los comandos disponibles
help :: IO()
help = do putStrLn "Comandos \n\t\tResolver un Hidato \n\tsolveHidato <hidato> <pos_inicial> <max_value>\n\n\t    <hidato>: debe ser una lista de listas que tenga este formato: [[Int]]\n\t    <pos_inicial>: debe de ser donde se encuentra el valor 1 en el Hidato con el siguiente formato (row,col,val), donde val tiene que ser igual a 1\n\t    <max_value>: Valor maximo que se encuentra en Hidato\n\tEjemplo:\n\tghci>m=[[0,0,4,0],[1,0,0,-1],[-1,0,0,9],[0,14,0,0]]\n\tghci>solveHidato m (1,0,1) 14\n\t"

----------------------------------------------------------------- Para testear ---------------------------------------------------------------
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

----------------------------------------------------------------- Para testear ---------------------------------------------------------------