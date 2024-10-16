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

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = (f x) : (map' f xs)

-- Práctica 0

promedio :: Fractional a => a -> a -> a
promedio x y = (x + y) / 2

maximo :: Ord a => a -> a -> a
maximo x y | x > y = x
           | otherwise = y

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- incN le suma n a cada elemento de la lista
incN :: Num t => t -> [t] -> [t]
incN n [] = []
incN n (x:xs) = (n + x) : incN n xs

-- a1 suma x e y. Solo funciona cuando y es mayor o igual a cero
a1 :: (Eq t1, Num t1, Num t2) => t2 -> t1 -> t2
a1 x 0 = x
a1 x y = a1 x (y - 1) + 1

-- a2 multiplica a x por y. Solo funciona bien si x e y son mayores o iguales a cero
a2 :: (Eq t, Num t, Num a) => a -> t -> a
a2 x 0 = 0
a2 x y = a2 x (y - 1) + x

-- a3 eleva a x a la y. Solo funciona si y es mayor o igual a cero
a3 :: (Eq t, Num t, Num a) => a -> t -> a
a3 x 0 = 1
a3 x y = a3 x (y - 1) * x

inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso n = Just (1 / n)

aEntero :: Either Int Bool -> Int
aEntero x = case x of
        (Left n) -> n
        (Right b) -> if b then 1 else 0