-- Ejercicio 1

-- max2 no está currificada
max2 :: (Float, Float) -> Float
max2 (x, y) | x >= y = x
            | otherwise = y
-- max2' es la versión currificada de max2
max2' :: Float -> Float -> Float
max2' x y | x >= y = x
          | otherwise = y

-- normaVectorial no está currificada
normaVectorial :: (Float, Float) -> Float
normaVectorial (x, y) = sqrt (x^2 + y^2)
-- normaVectorial' es la versión currificada de normaVectorial
normaVectorial' :: Float -> Float -> Float
normaVectorial' x y =  sqrt (x^2 + y^2)

-- subtract' está currificada
subtract' :: Float -> Float -> Float
subtract' = flip (-)
-- El flip hace que el primer argumento sea el que se va a restar, es decir subtract a b = b - a

-- predecesor está currificada ya que toma un único argumento
predecesor :: Float -> Float
predecesor = subtract' 1
-- Le puse el Main adelante de subtract para que no se equivoque con el subtract del Preludio de Haskell

-- evaluarEnCero está currificada ya que toma un único argumento
evaluarEnCero :: (Float -> b) -> b
evaluarEnCero = \f -> f 0

-- dosVeces está currificada
dosVeces :: (a -> a) -> a -> a
dosVeces = \f -> f . f

-- flipAll está currificada ya que toma un único argumento
flipAll :: [a -> b -> c] -> [b -> a -> c]
flipAll = map flip

-- flipRaro está currificada
flipRaro :: b -> (a -> b -> c) -> a -> c
flipRaro = flip flip


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
sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0

-- elem x ys se verifica si x pertenece a ys
-- elem :: (Foldable t, Eq a) => a -> t a -> Bool
  -- elem 3 [4, 5, 3] / output: True
  -- elem 9 [4, 5, 3] / output: False
  -- elem 'o' "Hola" / output: True
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = foldr (\y acc -> (x == y) || acc) False

-- xs ++ ys es la concatenación de xs e ys
  -- [4] ++ [5, 3] / output: [4, 5, 3]
  -- "Ho" ++ "la" / output: "Hola"
-- ++ ya está definido en el preludio
{-
(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs
-}

-- filter p xs devuelve la lista de elementos de la lista xs que verifican el predicado p
-- filter :: (a -> Bool) -> [a] -> [a]
  -- filter even [3,4,6,7,5,0] / output: [4,6,0]
  -- filter (<6) [3,4,6,7,5,0] / output: [3,4,5,0]
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

-- map f xs devuelve la lista obtenida aplicado f a cada elemento de xs
-- map :: (a -> b) -> [a] -> [b]
  --  map (^2) [3,10,5] / output: [9,100,25]
  -- map even [3,10,5] / output: [False,True,False]
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> (f x) : acc) []
-- map' f = foldr ((:) . f) []      (otra versión de map definida con foldr)

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
-- la función last devuelve el último elemento de la lista

sumasParciales :: Num a => [a] -> [a]
sumasParciales = foldl (\acc x -> acc ++ (if null acc then [x] else [x + last acc])) []

-- iv)
sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (-) 0

-- otra versión de sumaAlt:
sumaAlt' :: Num a => [a] -> a
sumaAlt' = foldr (\x acc -> -acc + x) 0

-- v)
sumaAltRev :: Num a => [a] -> a
sumaAltRev = foldl1 (flip (-))

-- otra versión de sumaAltRev:
sumaAltRev' :: Num a => [a] -> a
sumaAltRev' = foldl1 (\acc x -> -acc + x)


-- Ejercicio 4

-- i)
-- concatMap toma una función que produce una lista a partir de cada elemento de la lista original y luego concatena todas esas listas en una sola lista. Es como hacer un map seguido de un concat.
{-
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = concat . map f
-}
-- concatMap (\x -> [x, x+1]) [1, 2, 3]   /   output: [1, 2, 2, 3, 3, 4]

-- take toma los primeros n elementos de una lista. Si la lista tiene menos de n elementos, devuelve la lista completa.
-- take :: Int -> [a] -> [a]
-- take 3 [1, 2, 3, 4, 5]  /  output: [1, 2, 3]

-- drop elimina los primeros n elementos de una lista. Si la lista tiene menos de n elementos, devuelve una lista vacía.
-- drop :: Int -> [a] -> [a]
-- drop 3 [1, 2, 3, 4, 5]   / output: [4, 5]

permutaciones :: [a] -> [[a]]
permutaciones = foldr (\x rs -> concatMap (\r -> map (insert x r) [0..length r]) rs) [[]]
                where insert x r i = drop i r ++ [x] ++ take i r

-- ii)
partes :: [a] -> [[a]]
partes = foldr (\x rec -> rec ++ map (x:) rec) [[]]

-- versión alternativa de partes usando recr:
partes' :: [a] -> [[a]]
partes' xs = recr f [[]] xs
            where f x xs acc = acc ++ map (x:) acc

-- iii)
prefijos :: [a] -> [[a]]
prefijos = foldr (\x acc -> [] : map (x:) acc) [[]]

-- versión alternativa de prefijos usando take:
prefijos' :: [a] -> [[a]]
prefijos' xs = [take i xs | i <- [0.. (length xs)]]

-- iv)
sublistas :: [a] -> [[a]]
sublistas = recr (\x xs r -> map (x :) (prefijos xs) ++ r) [[]]

sublistas' :: [a] -> [[a]]
sublistas' xs = [[]] ++ [take j (drop i xs) | i <- [0.. (length xs)], j <- [1.. (length xs) - i]] 


-- Ejercicio 5

elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs then [x] else x : elementosEnPosicionesPares (tail xs)

-- elementosEnPosicionesPares no está escrita utilizando recursión estructural. Mas bien, utiliza recursión primitiva, ya que la recursión no se hace sobre todo xs, si no sobre tail xs, y se utiliza null xs como condición.

-- La función entrelazar toma dos listas y alterna los elementos de ambas listas en una sola lista
entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys then x : entrelazar xs [] else x : head ys : entrelazar xs (tail ys)

-- entrelazar utiliza recursión etructural

-- versión de entrelazar definida con foldr:
entrelazar' :: [a] -> [a] -> [a]
entrelazar' = foldr (\x fr ys -> if null ys then x : fr [] else x : head ys : fr (tail ys)) id


-- Ejercicio 6

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x:xs) = f x xs (recr f z xs)

-- a)
sacarUna :: Eq a => a -> [a] -> [a]
sacarUna n = recr (\x xs rec -> if x == n then xs else x : rec) []

-- b)
{-
No es conveniente aplicar foldr para definir sacarUna ya que por un lado foldr procesa toda la lista, y la funcion sacarUna solo necesita seguir hasta encontrar la primera aparición del elemento a eliminar. Por el otro lado, foldr procesa de derecha a izquierda, y el fin de sacarUna es eliminar la aparición del elemento buscado más a la izquierda.
-}

-- c)
insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado  n = recr (\x xs rec -> if n <= x then n : x : xs else x : rec) [n]


-- Ejercicio 7

-- i)
genLista :: a -> (a -> a) -> Int -> [a]
genLista n f i = take i (iterate f n)
-- La función iterate f x genera una lista infinita aplicando repetidamente la función f al valor inicial x