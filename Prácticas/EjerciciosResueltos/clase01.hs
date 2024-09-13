curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

prod :: Int -> Int -> Int
prod x y = x * y

doble :: Int -> Int
doble x = prod 2 x

doble' :: Int -> Int
doble' = prod 2

triple :: Float -> Float
triple = (*) 3.0

esMayorDeEdad :: Int -> Bool
esMayorDeEdad = (<=) 18

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g x = f (g x)

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

($) :: (a -> b) -> a -> b
($) f x = f x

const :: a -> b -> a
const x _ = x
-- const' x = (\y -> x)

divisiblePor2 :: Int -> Bool
divisiblePor2 = (== 0) Prelude.. (Prelude.flip mod 2)

maximo :: Ord a => [a] -> a
maximo (x:[]) = x
maximo (x:xs) = if x > maximo xs
                then x
                else maximo xs

maximo' :: Ord a => [a] -> a
maximo' = mejorSegun (>)

minimo :: Ord a => [a] -> a
minimo (x:[]) = x
minimo (x:xs) = if x < minimo xs
                then x
                else minimo xs

listaMasCorta :: [[a]] -> [a]
listaMasCorta (x:[]) = x
listaMasCorta (x:xs) = if length x < length (listaMasCorta xs)
                       then x
                       else listaMasCorta xs

listaMasCorta' :: [[a]] -> [a]
listaMasCorta' = mejorSegun (\x y -> length x < length y)

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f [x] = x
mejorSegun f (x:y:xs) = if f x y
                        then mejorSegun f (x:xs)
                        else mejorSegun f (y:xs)
-- la segunda parte de mejorSegun también se puede escribir como:
-- mejorSegun f (x:xs) | f x (mejorSegun f xs) = x
--                     | otherwise = mejorSegun f xs

-- A PARTIR DE ACA ME FALTA AGREGAR LAS FUNCIONES AL PDF!!!!!!!!

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs) = if p x
                  then x : Main.filter p xs
                  else Main.filter p xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x rec -> if p x then (:) x rec else rec) []

-- otra manera de escribir filter' con foldr:
-- filter' :: (a -> Bool) -> [a] -> [a]
-- filter' p = foldr (\x -> if p x then (:) x else id) [] 


deLongitudN :: Int -> [[a]] -> [[a]]
deLongitudN n = Prelude.filter (\x -> length x == n)

-- otra manera de escribir deLongitudN:
-- deLongitudN n = filter ((== n) . length)

soloPuntosFijosEnN :: Int -> [Int->Int] -> [Int->Int]
soloPuntosFijosEnN n = Prelude.filter (\f -> f n == n)

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : Main.map f xs

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x rec -> f x : rec) []

-- otra manera de escribir map' uando foldr:
-- map' f = foldr ((:) . f) []

reverseAnidado :: [[Char]] -> [[Char]]
reverseAnidado = reverse Prelude.. Prelude.map reverse

paresCuadrados :: [Int] -> [Int]
paresCuadrados = Prelude.map (\x -> if even x then x * x else x)

listaComp :: (a -> b) -> [a] -> (a -> Bool) -> [b]
listaComp f xs p = [f x | x <- xs, p x]

listaComp' :: (a -> b) -> [a] -> (a -> Bool) -> [b]
listaComp' f xs p = Prelude.map f (Prelude.filter p xs)

mejorSegun' :: (a -> a -> Bool) -> [a] -> a
mejorSegun' f = foldr1 (\x y -> if f x y then x else y)

mejorSegun'' :: (a -> a -> Bool) -> [a] -> a
mejorSegun'' f = foldl1 (\x y -> if f x y then x else y)
