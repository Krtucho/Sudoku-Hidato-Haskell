module Main where
import Lib

-- import PdePreludat

main :: IO ()
main = someFunc



f :: Int -> Bool
f 3 = True
f _ = False

doubleMe x = x + x
-- doubleUs x y = x*2 + y*2
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
    then x
    else x * 2

doubleSmallNumber' x = (if x > 100
    then x
    else x * 2) + 1

factorial :: Integer -> Integer
-- factorial n = product [1..n]

factorial 0 = 1
factorial n = n * factorial (n - 1)

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)

-- !!!!!!!!!!!!!!!!!! Aqui comienza la cp1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- CP 1 Ej 2
five x = 5

aply f v = f v

id':: (Num a) => a -> a
id' x = x

first :: (Num a, Num b) => (a,b) -> a
first (a, _) = a

-- e
derive f x = (f (x + 0.001) - f x) / 0.001

-- f
sign' :: (Num a) => a -> a
sign' x = -x

-- g
abs' :: Float -> Float
abs' a | a < 0 = a * (-1)
       | a > 0 =  a
       | otherwise = 0

abs_sign' :: Float -> Float
abs_sign' a | a < 0 = sign' a
       | a > 0 =  a
       | otherwise = 0
-- h
xor :: Integer -> Integer -> Integer
xor x y | x == 0 && y == 0 = 0
        | otherwise = 1

-- i
max2:: Integer -> Integer -> Integer
max2 a b | a > b = a
         | otherwise = b

max3 :: Integer -> Integer -> Integer -> Integer
max3 a b c | max2 a b == a = max2 a c
           | otherwise = max2 b c

-- j
-- swap :: (a,a) -> (a,a)
swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

-- k
sumsqrt :: Integer -> Integer
sumsqrt n = sum [x * x| x <- [1..n]]

-- Ej3
-- a
-- Int -> bool
-- even x

-- c
-- (Int, Int) -> Int
-- linearizeUppertri
-- guessMSSize

-- d
-- Int -> (Int, Int)
-- split
-- quotRem28

-- e
-- a -> Bool
-- isBottom
-- ignore

-- f
-- a -> a
-- id
-- inline
-- noinline
-- lazy


-- Ej4
-- a)
suma :: [Integer] -> Integer
suma [] = 0
suma [x] = x
suma (x:xs) = x + suma xs

-- b)
alguno :: [Bool] -> Bool
alguno [] = False
alguno [x] = x
alguno (x:xs) = x || alguno xs

-- c)
todos :: [Bool] -> Bool
todos [] = True
todos [x] = x
todos (x:xs) = x && todos xs

-- d)
mult :: [Integer] -> Integer
mult [] = 0
mult [x] = x
mult (x:xs) = x * mult xs

-- e)
restos :: [Integer] -> Integer -> [Integer]
restos [] n = []
restos (x:lst) n = (mod x n) : (restos lst n)

-- f)
cuadrados :: [Integer] -> [Integer]
cuadrados [] = []
cuadrados (x:lst) = (x^2) : (cuadrados lst)

-- g
longitudes :: [[a]] -> [Int]
longitudes lst = [length x | x <- lst]

-- h
orden :: [(Int,Int)] -> [(Int,Int)]
orden xs = [x | x <- xs, fst x < (3 * (snd x))]

-- i
pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) | ((mod x 2) == 0) = x:(pares xs)
                      | otherwise = pares xs

-- j
letras :: [Char] -> [Char]
letras [] = []
letras (x:xs) | x `elem` ['a'..'z'] || x `elem` ['A'..'Z'] = x:letras xs
              | otherwise = letras xs

-- k
masDe :: [[a]] -> Int -> [[a]]
masDe xxs n = [x | x <- xxs, length x > n]
-- Another way..
-- masDe [] n = []
-- masDe (x:xs) n | length x > n = x:masDe xs n
--                | otherwise = masDe xs n

-- !!!!!!!!!!!!!!!!!!!!  Aqui comienza la CP2  !!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- CP 2

add :: Int -> (Int -> Int)
add x y = x + y


incrementa :: Int -> Int
incrementa = add 1

-- Ej 1
-- a)
collatz :: Int -> [Int]
collatz 1 = [1]
collatz n = if even n then (n) : collatz (div n 2) else n  : collatz  (n * 3 + 1)

-- b)
collatzSeqLeng ::  Int -> Int -> [[Int]]
collatzSeqLeng n m = [x | x <- map collatz [1..n], length x == m]

-- 2
cumplen :: (a -> Bool) -> [a] -> Bool
cumplen p xs = all p xs

-- 3
-- a
divn :: Int -> [Int]
divn n = filter f [2..n] where f x = mod n x == 0

esPrimo :: Int -> Bool
esPrimo 1 = True
esPrimo 2 = True
esPrimo n = length (divn n ) == 1

-- Another way... Esta es la q mas me cuadra
-- esPrimo :: Int -> Bool
-- esPrimo 2 = True
-- esPrimo n = length (filter (\x -> mod n x == 0) [1..n]) == 2

-- b
primos :: Int -> [Int]
primos n = [x | x <- [1..n], esPrimo x]

-- c
prod :: [Int] -> Int
prod = foldr (*) 1

-- Another way...I preffer the last one
-- prod [] = 1
-- prod (x:xs) =  x * prod xs

-- d
-- factores :: Int -> [Int]
-- factores n = fact <= [x | x <- [1..n], esPrimo x] where (prod fact) == n

-- CP Listas
-- El 1 es el 2 de los anteriores

-- 2

-- a
-- No sirve aun
pal :: String -> Bool
pal [x] = True
pal (x:word) = x == (last word) && pal (init word)
    -- | (mod (length word) 2) == 0 = all (\x y -> x == y) word (reverse word)
    --         | otherwise = div (length word) 2
-- pal word | mod length word == 2 = all (\x y-> x == y)[1..n][] --length word == length [x | x <- word, y <- reverse word, x == y]

-- b
longPro :: [[a]] -> Int
longPro xs = div (sum [length x | x <- xs]) (length xs)

-- c
adyac :: [Int] -> [(Int, Int)]
adyac xs = zip xs (tail xs)--[(x, y) | x <- xs, y <- tail xs]

--d
remDups:: [Int] -> [Int]
remDups [x] = [x]
remDups (x:y:xs) | x == y = x:(remDups xs)
                 | otherwise = x:y:remDups xs

--e
takeUntil :: [a] -> (a -> Bool) -> [a]
takeUntil [] p = []
takeUntil (x:xs) p = if not (p x) then (x:takeUntil xs p) else []
-- takeUntil xs p = takeWhile not (p xs)

-- Ej 3
data Persona = Persona {nombre :: String,
                        apellido1 :: String,
                        apellido2 :: String,
                        edad :: Int
                        } deriving (Show)

-- a
aps_iguales :: [Persona] -> [Persona]
aps_iguales xs = filter (\x -> apellido1 x == apellido2 x) xs
--  Anogher way...
-- aps_iguales xs = [x | x <- xs, apellido1 x == apellido2 x]

-- b
tienen_ap :: [Persona] -> String -> [Persona]
tienen_ap xs ap = filter (\x -> apellido1 x == ap ||  apellido2 x == ap) xs

-- c
ninnos :: [Persona] -> [Persona]
ninnos xs = filter (\x -> edad x < 11) xs

--------------------------------------------- Aqui comienza la CP 3 ------------------------------------------------------
-- Cp 3
-- Ej 1
data ABB = ABB {valor:: Int, hi:: ABB, hd:: ABB} | Nil deriving(Eq, Ord, Show)

-- 1.1
minimo :: ABB -> Int
minimo Nil = maxBound :: Int
minimo tree = min (minimo (hi tree)) (valor tree)
    --  --| min (hi tree) 'elem' [Nil] = valor tree
    --         otherwise = min (minimo (hi tree)) (valor tree)

-- 1.2
pertenece :: Int -> ABB -> Bool
pertenece x Nil = False
pertenece x tree | x == y = True
                 | x < y  = pertenece x (hi tree)
                 | x > y  = pertenece x (hd tree)
                 where y = valor tree

-- 1.3
altura :: ABB -> Int
altura Nil = 0
altura tree |  ((hi tree) == Nil) && ((hd tree) == Nil) = 1
            | otherwise = 1 + max (altura (hi tree)) (altura (hd tree))

balanced :: ABB -> Bool
balanced Nil = True
balanced tree | abs (altura (hi tree) - altura (hd tree)) <= 2 = True
              | otherwise = False

-- 1.4
successor :: Int -> ABB -> ABB
successor x Nil = Nil
successor x tree | (x >= (valor tree)) = successor x (hd tree)
                 | p == Nil = tree
                 | otherwise = p
                 where p = successor x (hi tree)

-- Caso de prueba
-- successor 2 ABB {valor=0, hi=Nil, hd=ABB {valor=4, hi=ABB {valor=3, hi=Nil, hd=Nil}, hd=Nil}}

-- menorDeLosMayores x Nil = Nil
-- menorDeLosMayores x tree | x == y = menorDeLosMayores hd tree
--                          | x < y  = menorDeLosMayores hd tree--ABB{valor=y, hi = insertar x (hi tree), hd = hd tree}
--                          | x > y  = menorDelosMayores hi tree --ABB{valor=y, hi=hi tree, hd = insertar x (hd tree)}
--                          where y = valor tree

-- 1.5
insertar :: Int -> ABB -> ABB
insertar x Nil= ABB{valor=x, hi = Nil, hd = Nil}
insertar x tree | x == y = tree
                | x < y  = ABB{valor=y, hi = insertar x (hi tree), hd = hd tree}
                | x > y  =ABB{valor=y, hi=hi tree, hd = insertar x (hd tree)}
                where y = valor tree
                

-- Ej 2
data Seq a = Nul | Unit a | Cat (Seq a) (Seq a) deriving(Show)

-- a
appSeq :: Seq a -> Seq a -> Seq a
appSeq seq1 seq2 = Cat seq1 seq2

-- b
consSeq :: a -> Seq a -> Seq a
consSeq x seq1 = Cat (Unit x) seq1

-- c
lenSeq :: Seq a -> Int
lenSeq Nul = 0
lenSeq (Unit a) = 1
lenSeq (Cat (a) (b)) = (lenSeq a) + (lenSeq b)

-- d
revSeq :: Seq a -> Seq a
revSeq Nul = Nul
revSeq (Unit a) = (Unit a)
revSeq (Cat (seq1) (seq2)) = Cat (revSeq seq2) (revSeq seq1)

-- e
headSeq :: Seq a -> Seq a
headSeq Nul = Nul
headSeq (Unit seq1) = (Unit seq1)
headSeq (Cat seq1 seq2) = headSeq seq1

-- f
-- tailSeq :: Seq a -> Seq a
-- tailSeq Nul = Nul
-- tailSeq (Unit u) = Nul
-- tailSeq (Cat (Cat (seq1 seq2) seq2))
-- tailSeq (Cat seq1 seq2) = Cat seq2

-- Ej3
data Bag a = Nulo | 
             Elem (a, Int) (Bag a) deriving(Show)

-- insert x Nil = Elem (x,1) Nil
-- insert x (Elem(e, n) b) | x  == e = Elem(e, n + 1) b
--                         | otherwise = Elem(e,n) (insert x b)

-- list2bag :: (Eq a) => [a] -> Bag
-- list2bag (x:xs) = insert x (list2bag xs)

-- bagEmpty :: Bag -> Bool
-- bagEmpty Nil = True
-- bagEmpty _ = False

-- bagCount :: Bag -> Int
-- bagCount Nil = 0 
-- bagCount Elem(_, n) rb = n + bagCount rb

-- -- bagElem :: Bag -> a -> Bool
-- -- bagElem Nil x = False
-- -- bagElem (Elem(e,_) b) x | x == e = True
-- --                         | otherwise bagElem b x

-- bagEquals :: Bag a -> Bag a -> Bool
-- bagEquals (Elem(e1, n1) b1) (Elem(e2, n2) b2) = bagSubbag (Elem(e1, n1) b1) (Elem(e2, n2) b2) && bagSubbag (Elem(e2, n2) b2) (Elem(e1, n1) b1) 

-- bagElemTimes :: (a,Int)-> Bag a -> Bool
-- bagElemTimes (e, n) Nil = False
-- bagElemTimes (e1, n1) (Elem(e2, n2) b)  | e1== e2 && n1<=n2 = True
--                                         | otherwise = bagElemTimes (e1, n1) b

-- bagSubbag :: Bag a -> Bag a -> Bool
-- -- bagSubbag Nil Nil = True
-- bagSubbag  _ Nil = False
-- bagSubbag  Nil _ = False
-- bagSubbag (Elem(e1, n1) b1) (Elem(e2, n2) b2) = bagElemTimes (e1, n1) (Elem(e2, n2) b2) && bagSubbag b1 (Elem(e2, n2) b2)
--(e == e) && (n == n) = bagSubbag b1 (Elem(e2, n2) b2) 
                                               

-- bagInter :: Bag a -> Bag a -> 

-- funcZip xs ys zs = [x | x <- zip (zip xs ys) zs]

-- test xs = filter (\(x,y) -> x > y) xs