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
-- uncurry ya viene definida en el preludio
{-
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y
-}

-- iii)
{-
La respuesta teórica es que no es posible en Haskell "curriar" una función con un número arbitrario de argumentos, directamente. Esto se debe a que en Haskell cada función tiene un tipo bien definido, y el número de argumentos debe conocerse estáticamente.

Sin embargo, se puede emular esta funcionalidad usando una lista o una estructura que represente los argumentos y que se pueda recorrer con funciones de orden superior. Aquí hay una aproximación práctica:
-}
curryN :: ([a] -> b) -> (a -> [a] -> b)
curryN f x = \xs -> f (x:xs)

-- Ejemplo:
f :: [Int] -> Int
f xs = sum xs

g = curryN f
-- g 1 [2,3]  -- Equivale a `f (1:[2,3])` -> 6


-- Ejercicio 3

-- i)
-- sum xs suma todos los elementos de la lista xs
  -- sum [3,2,5] / output: 10
  -- sum [] / output: 0
sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0

-- elem x ys verifica si x pertenece a ys
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
  -- filter even [3,4,6,7,5,0] / output: [4,6,0]
  -- filter (<6) [3,4,6,7,5,0] / output: [3,4,5,0]
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

-- map f xs devuelve la lista obtenida aplicado f a cada elemento de xs
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

-- versión alternativa de sublistas usando take, drop y length:
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
-- La función iterate f n genera una lista infinita aplicando repetidamente la función f al valor inicial n

-- versión alternativa de genLista definida con foldr:
genLista' :: a -> (a -> a) -> Int -> [a]
genLista' n f i = foldr (\x rec -> if null rec then [n] else rec ++ [f (last rec)]) [] [1.. i]

-- ii)
-- desdeHasta devuelve una lista desde i hasta f inclusive
desdeHasta :: Int -> Int -> [Int]
desdeHasta i f = genLista i (+1) (f - i + 1)


-- Ejercicio 8

-- i)
mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f = map (uncurry f)

{-
Un ejemplo de como usar mapPares es:
f :: Int -> Int -> Int
f x y = x + y
y queres hacer:
mapPares f [(1, 2), (3, 4)]   /   output: [3, 7]
-}

-- ii)
armarPares :: [a] -> [b] -> [(a, b)]
armarPares = foldr (\x rec ys -> if null ys then [] else (x, head ys) : rec (tail ys)) (const [])

-- versión alternativa de armarPares definida usando recr:
armarPares' :: [a] -> [b] -> [(a, b)]
armarPares' = recr (\x xs rec ys -> case ys of
    [] -> []
    (y:ys') -> (x, y) : rec ys') (const [])

{-
Esta función ya existe en el preludio y se llama zip.
La función toma dos listas y devuelve una lista de pares. Cada par contiene elementos correspondientes de las dos listas originales. Si una lista es más corta que la otra, la longitud de la lista resultante será la de la lista más corta.
-}

-- iii)
mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f = recr (\x xs rec ys -> case ys of
                                 [] -> []
                                 (y:ys') -> f x y : rec ys') (const [])

-- versión de mapDoble definida usando foldr:
mapDoble' :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble' f = foldr (\x rec ys -> if null ys then [] else f x (head ys) : rec (tail ys)) (const [])

-- versión de mapDoble definida usando zip y mapPares:
mapDoble'' :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble'' f xs ys = mapPares f (zip xs ys)

{-
Ya existe en el preludio de haskell una función que hace lo mismo que mapDoble. Se llama zipWith.
zipWith en Haskell toma una función y dos listas, y devuelve una lista que resulta de aplicar la función a elementos correspondientes de las dos listas
-}


-- Ejercicio 9

-- i)
sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat = zipWith (zipWith (+))

-- ii)
trasponer :: [[Int]] -> [[Int]]
trasponer = foldr (zipWith (:)) (repeat [])

-- Esta función utiliza repeat [] para crear una lista infinita de listas vacías, que será truncada por zipWith (:) a la longitud adecuada durante la transposición


-- Ejercicio 10

-- i)
foldNat :: a -> (a -> a) -> Integer -> a
foldNat cBase fRec 0 = cBase
foldNat cBase fRec n = fRec (foldNat cBase fRec (n - 1))

-- ii)
potencia :: Integer -> Integer -> Integer
potencia base exponente = foldNat 1 (* base) exponente


-- Ejercicio 11

data Polinomio a = X | Cte a | Suma (Polinomio a) (Polinomio a) | Prod (Polinomio a) (Polinomio a)

foldPol :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b
foldPol cX cCte cSuma cProd pol = case pol of
    X              -> cX
    Cte c          -> cCte c
    Suma p1 p2     -> cSuma (foldPol cX cCte cSuma cProd p1) (foldPol cX cCte cSuma cProd p2)
    Prod p1 p2     -> cProd (foldPol cX cCte cSuma cProd p1) (foldPol cX cCte cSuma cProd p2)

evaluar :: Num a => a -> Polinomio a -> a
evaluar valor = foldPol valor id (+) (*)

-- Polinomio de ejemplo:
p :: Polinomio Int
p = Suma (Suma (Prod (Cte 3) (Prod X X)) (Prod (Cte 2) X)) (Cte 5)


-- Ejercicio 12

data AB a = Nil | Bin (AB a) a (AB a)

-- para poder usar print y ver la estructura del arbol:
instance Show a => Show (AB a) where
    show Nil = "Nil"
    show (Bin izq r der) = "Bin (" ++ show izq ++ ") " ++ show r ++ " (" ++ show der ++ ")"


-- Arboles binarios de prueba:
arbolDeEnteros :: AB Int
arbolDeEnteros = Bin (Bin (Bin Nil 3 Nil) 2 (Bin Nil 4 Nil)) 1 (Bin (Bin Nil 6 Nil) 5 (Bin Nil 7 Nil))

arbolBBDeEnteros :: AB Int
arbolBBDeEnteros = Bin (Bin (Bin Nil 1 Nil) 3 (Bin Nil 4 Nil)) 5 (Bin (Bin Nil 6 Nil) 8 (Bin Nil 9 Nil))

arbolVacio :: AB a
arbolVacio = Nil

-- i)
-- Recursión estructural:
foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB cNil cBin Nil = cNil
foldAB cNil cBin (Bin i r d) = cBin (foldAB cNil cBin i) r (foldAB cNil cBin d)

-- Recursión primitiva:
recAB :: b -> (AB a -> b -> a -> AB a -> b -> b) -> AB a -> b
recAB cNil cBin Nil = cNil
recAB cNil cBin (Bin i r d) = cBin i (recAB cNil cBin i) r d (recAB cNil cBin d)

-- ii)
esNil :: AB a -> Bool
esNil arbol = case arbol of
    Nil -> True
    _ -> False

-- versión de esNil usando foldAB:
esNil' :: AB a -> Bool
esNil' = foldAB True (\_ _ _ -> False)

altura :: AB a -> Int
altura = foldAB 0 (\izq _ der -> 1 + max izq der)

cantNodos :: AB a -> Int
cantNodos = foldAB 0 (\izq _ der -> 1 + izq + der)

-- iii)
mejorSegunAB :: (a -> a -> Bool) -> AB a -> a
mejorSegunAB _ Nil = error "El árbol no puede estar vacío"
mejorSegunAB f (Bin i r d) = recAB r (\_ recI r _ recD -> mejor r (mejor recI recD)) (Bin i r d)
  where mejor x y = if f x y then x else y

-- versión alternativa de mejorSegunAB usando foldAB:
mejorSegunAB' :: (a -> a -> Bool) -> AB a -> a
mejorSegunAB' _ Nil = error "El árbol no puede estar vacío"
mejorSegunAB' f (Bin i r d) = foldAB r (\izq r der -> (izq `g` r) `g` der) (Bin i r d)
    where g x y = if f x y then x else y

-- iv)
esABB :: Ord a => AB a -> Bool
esABB = recAB True (\izq recI r der recD -> recI && recD && all (<= r) (valores izq) && all (> r) (valores der))

-- Función auxiliar para obtener todos los valores de un árbol
valores :: AB a -> [a]
valores Nil = []
valores (Bin izq r der) = valores izq ++ [r] ++ valores der

-- v) Para esNil, altura y cantNodos usé foldAB ya que son operaciones acumulativas sobre la estructura completa. En cambio para mejorSegunAB y esABB es mejor usar recAB ya que te permite acceder directamente a los subárboles durante la evaluación.


-- Ejercicio 13

-- i)
ramas :: AB a -> [[a]]
ramas = foldAB [] (\izq r der -> if null izq && null der then [[r]] else map (r :) (izq ++ der))

cantHojas :: AB a -> Int
cantHojas = foldAB 0 (\izq _ der -> if izq == 0 && der == 0 then 1 else izq + der)

espejo :: AB a -> AB a
espejo = foldAB Nil (\izq r der -> Bin der r izq)

-- ii)
mismaEstructura :: AB a -> AB b -> Bool
mismaEstructura t1 t2 = compararEstructura (ramas t1) (ramas t2)

-- Función auxiliar para comparar las ramas de ambos árboles
compararEstructura :: [[a]] -> [[b]] -> Bool
compararEstructura ramas1 ramas2 = length ramas1 == length ramas2 && all igualLongitud (zip ramas1 ramas2)
  where
    igualLongitud (r1, r2) = length r1 == length r2


-- Ejercicio 14

-- arboles que solo contienen información en sus hojas:
data AIH a = Hoja a | Bin' (AIH a) (AIH a)
-- puse Bin' para que no haya conflicto con la estructura  de arbol binario

-- para que pueda imprimir los arboles:
instance Show a => Show (AIH a) where
    show (Hoja x) = "Hoja " ++ show x
    show (Bin' izq der) = "Bin (" ++ show izq ++ ") (" ++ show der ++ ")"

-- ejemplos de arbol AIH:

arbolAIHDeEnteros = Bin' (Hoja 1) (Bin' (Hoja 2) (Hoja 3))
arbolAIHDeUnaHoja = Hoja 3

-- a)
foldAIH :: (a -> b) -> (b -> b -> b) -> AIH a -> b
foldAIH cHoja cBin' (Hoja a) = cHoja a
foldAIH cHoja cBin' (Bin' izq der) = cBin' (foldAIH cHoja cBin' izq) (foldAIH cHoja cBin' der)

-- b)
alturaAIH :: AIH a -> Int
alturaAIH = foldAIH (const 1) (\izq der -> 1 + max izq der)

tamaño :: AIH a -> Int
tamaño = foldAIH (const 1) (+)


-- Ejercicio 15

-- i)
data RoseTree a = Rose a [RoseTree a]

-- ejemplo de RoseTree:
arbolRT = Rose 1 [Rose 2 [Rose 4 [], Rose 5 []], Rose 3 []]

-- ii)
foldRT :: (a -> [b] -> b) -> RoseTree a -> b 
foldRT fRose (Rose n hijos) = fRose n (map rec hijos)
    where rec = foldRT fRose

-- iii)
-- a)
hojasRT :: RoseTree a -> [a]
hojasRT = foldRT (\n rec -> if null rec then [n] else concat rec)

-- b)
distancias :: RoseTree a -> [Int]
distancias = foldRT (\_ hijos -> if null hijos then [0] else map (+1) (concat hijos))

-- c)
alturaRT :: RoseTree a -> Int
alturaRT = foldRT (\_ hijos -> if null hijos then 1 else 1 + maximum hijos)
-- maximum encuentra el maximo en una lista no vacia


-- Ejercicio 16

data HashSet a = Hash (a -> Integer) (Integer -> [a])

-- i)
vacio :: (a -> Integer) -> HashSet a
vacio h = Hash h (const [])

-- ii)
pertenece :: Eq a => a -> HashSet a -> Bool
pertenece x (Hash h t) = x `elem` t (h x)

-- iii)
agregar :: Eq a => a -> HashSet a -> HashSet a
agregar x (Hash h t) | pertenece x (Hash h t) = (Hash h t)
                     | otherwise = Hash h (\i -> if i == h x then x : t i else t i)

-- iv)
intersección :: Eq a => HashSet a -> HashSet a -> HashSet a
intersección (Hash h1 t1) (Hash h2 t2) =
    Hash h1 (\i -> filter (`elem` t2 i) (t1 i))

-- v)
foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' _ [] = error "foldr1: lista vacía"
foldr1' f (x:xs) = foldr f x xs


-- Ejercicio 17

-- El valor de la expresión [x | x <- [1..3], y <- [x..3], (x + y) `mod` 3 == 0] es [1, 3]


-- Ejercicio 18

paresDeNat :: [(Int, Int)]
paresDeNat = [(x, y) | n <- [0..], x <- [0..n], let y = n - x]


-- Ejercicio 19

{-
pitagoricas = [(a, b, c) | a <- [1..], b <- [1..], c <- [1..], a^2 + b^2 == c^2]

La solución de arriba es ineficiente ya que todos los bucles (a <- [1..], b <- [1..], c <- [1..]) son infinitos y anidados. Esto hace que la lista tarde una eternidad en generar un único resultado.
-}

-- Una mejor solución sería:
pitagoricas :: [(Integer, Integer, Integer)]
pitagoricas = [(a, b, c) | c <- [1..], a <- [1..c-1], let b = floor (sqrt (fromIntegral (c^2 - a^2))), b > 0, a^2 + b^2 == c^2]


-- Ejercicio 20

listasQueSuman :: Int -> [[Int]]
listasQueSuman 0 = [[]]
listasQueSuman n = [x : xs | x <- [1..n], xs <- listasQueSuman (n - x)]

{-
La recursión en listasQueSuman no es estructural porque:
  - No trabaja sobre una estructura de datos concreta.
  - Reduce un valor escalar (n) mediante una estrategia no derivada de una estructura natural.
-}


-- Ejercicio 21

todasLasListas :: [[Int]]
todasLasListas = concatMap listasQueSuman [1..]


-- Ejercicio 22

-- a)
arbolesUnit :: [AIH ()]
arbolesUnit = concatMap generarNivel [1..]

generarNivel :: Int -> [AIH ()]
generarNivel 1 = [Hoja ()]
generarNivel n = [Bin' izq der | k <- [1..n-1], izq <- generarNivel k, der <- generarNivel (n-k)]

-- b)
{-
La razón principal por la que la recursión no es estructural es que no estamos descomponiendo la estructura del árbol (o cualquier otro tipo de dato) de una manera predecible y estructurada como lo haríamos con listas, que tienen una descomposición fija en su cabeza (x) y su cola (xs).

En cambio, estamos dividiendo el problema de generar árboles o procesar árboles a partir de la cantidad de nodos que queremos (n) o de una operación matemática en lugar de recorrer de forma estructural el árbol.
-}

