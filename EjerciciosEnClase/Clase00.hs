-- Teórica 0

-- Para empezar a usar haskell en la terminal ingresar ghci
-- Para preguntarle a la terminal un tipo de dato hat que escribir primero :t
-- Para cargar un archivo usar :l nombreDelArchivoSinExtensión
-- Para hacer refresh a un archivo usar :r
-- Para salir del ghci utilizar Ctrl D

doble :: Int -> Int
doble x = x + x

signo :: Int -> Bool
signo n | n >= 0 = True
        | otherwise = False

f :: (Int, Int) -> Int
f (x, y) = g x + y
        where g z = z + 2
-- f (1, 4) / output: 7

id :: a -> a
id x = x

curry :: ((a, b) -> c) -> (a -> b -> c)
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x
-- flip (:) [2, 3] 1 / output: [1, 2, 3]

(.) :: (b -> c) -> (a -> b) -> a -> c
(g . f) x = g (f x)

dobleL :: [Int] -> [Int]
dobleL [] = []
dobleL (x:xs) = (doble x) : (dobleL xs)

esParL :: [Int] -> [Bool]
esParL [] = []
esParL (x:xs) = (even x) : (esParL xs)

longL :: [[a]] -> [Int]
longL [] = []
longL (x:xs) = (length x) : (longL xs)