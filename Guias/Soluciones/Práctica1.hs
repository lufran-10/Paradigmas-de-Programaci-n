-- Ejercicio 1

max2 :: (Float, Float) -> Float
max2 (x, y) | x >= y = x
            | otherwise = y
-- max2 NO está currificada!!
max2' :: Float -> Float -> Float
max2' x y | x >= y = x
          | otherwise = y

normaVectorial :: (Float, Float) -> Float
normaVectorial (x, y) = sqrt (x^2 + y^2)
-- normaVectorial NO está currificada!!
normaVectorial' :: Float -> Float -> Float
normaVectorial' x y =  sqrt (x^2 + y^2)

subtract :: Float -> Float -> Float
subtract = flip (-)
-- subtract está currificada!!
-- El flip hace que el primer argumento sea el que se va a restar, es decir subtract a b = b - a

predecesor :: Float -> Float
predecesor = Main.subtract 1
-- predecesor está currificada ya que toma un único argumento!!
-- Le puse el Main adelante de subtract para que no se equivoque con el subtract del Preludio de Haskell

evaluarEnCero :: (Float -> b) -> b
evaluarEnCero = \f -> f 0
-- evaluarEnCero está currificada ya que toma un único argumento!!

dosVeces :: (a -> a) -> a -> a
dosVeces = \f -> f . f
-- dosVeces está currificada!!

flipAll :: [a -> b -> c] -> [b -> a -> c]
flipAll = map flip
-- flipAll está currificada ya que toma un único argumento!!

flipRaro :: b -> (a -> b -> c) -> a -> c
flipRaro = flip flip
-- flipRaro está currificada!!


-- Ejercicio 2

-- i)
curry :: ((a , b) -> c) -> (a -> b -> c)
curry f x y = f(x, y)

-- ii)
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

-- iii) FALTA RESOLVER!!!!


-- Ejercicio 3

-- i)
-- sum xs suma todos los elementos de la lista xs
-- sum :: (Foldable t, Num a) => t a -> a
  -- sum [3,2,5] / output: 10
  -- sum [] / output: 0
-- sum' [] = foldr (+) 0 []
-- sum' (x:xs) = foldr (+) 0 (x:xs)
sum' :: Num a => [a] -> a
sum' = foldr (+) 0

-- elem x ys se verifica si x pertenece a ys
  -- elem 3 [4, 5, 3] / output: True
  -- elem 9 [4, 5, 3] / output: False
  -- elem 'o' "Hola" / output: True
-- elem :: (Foldable t, Eq a) => a -> t a -> Bool
-- elem :: a -> [a] -> Bool
elem' :: Eq a => a -> [a] -> Bool
elem' x = foldr (\y acc -> (x == y) || acc) False

-- xs ++ ys es la concatenación de xs e ys
  -- [4] ++ [5, 3] / output: [4, 5, 3]
  -- "Ho" ++ "la" / output: "Hola"
-- (++) :: [a] -> [a] -> [a]
(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs
-- En las pruebas tengo que usar Main.++ para no usar el ++ del Preludio

-- filter p xs devuelve la lista de elementos de la lista xs que verifican el predicado p
  -- filter even [3,4,6,7,5,0] / output: [4,6,0]
  -- filter (<6) [3,4,6,7,5,0] / output: [3,4,5,0]
-- filter :: (a -> Bool) -> [a] -> [a]
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

-- map f xs devuelve la lista obtenida aplicado f a cada elemento de xs
  --  map (^2) [3,10,5] / output: [9,100,25]
  -- map even [3,10,5] / output: [False,True,False]
-- map :: (a -> b) -> [a] -> [b]
map' :: (a -> b) -> [a] -> [b]
-- map' f = foldr (\x acc -> (f x) : acc) []  ESTA ES VÁLIDA TAMBIÉN AUNQUE MENOS SIMPLIFICADA QUE LA DE ABAJO
map' f = foldr ((:) . f) []

-- ii)
-- foldr (fold right) es una función de plegado que toma tres argumentos:
  -- Una función binaria que toma dos argumentos (un elemento de la lista y un acumulador) y produce un nuevo acumulador.
  -- Un valor inicial (o acumulador inicial) que sirve como punto de partida para la reducción.
  -- Una lista sobre la que se realiza la operación de plegado.
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- En cambio foldr1 es parecida pero:
  -- No toma un valor inicial explícito; en cambio, usa el primer elemento de la lista como el valor inicial.
  -- Solo se puede usar con listas no vacías, ya que necesita al menos un elemento para funcionar.
-- foldr1 :: (a -> a -> a) -> [a] -> a
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x y -> if f x y then x else y)

maximo :: Ord a => [a] -> a
maximo = mejorSegun (>)

-- iii)
-- foldl (fold left) es una función en Haskell que se utiliza para reducir una lista a un único valor aplicando una función binaria de manera acumulativa desde la izquierda.
-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- argumentos:
  -- Una función que toma dos argumentos: el acumulador (b) y un elemento de la lista (a). La función debe devolver un nuevo acumulador (b).
  -- Un valor inicial que sirve como punto de partida para el acumulador.
  -- La lista sobre la cual se realizará el plegado.

sumasParciales :: Num a => [a] -> [a]
sumasParciales = foldl (\acc x -> acc Prelude.++ (if null acc then [x] else [x + last acc])) []

-- iv)
sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (\x acc -> -acc + x) 0

-- v)
sumaAltRev :: Num a => [a] -> a
sumaAltRev = foldl (\acc x -> -acc + x) 0


-- Ejercicio 4 (FALTA HACER!!)


-- Ejercicio 5

elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs then [x] else x : elementosEnPosicionesPares (tail xs)

-- Esta función está escrita utilizando recursión estructural.
