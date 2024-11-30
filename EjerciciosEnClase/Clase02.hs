-- Práctica 2

{-
Recursión estructural:
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x : xs) = f x (foldr f z xs)
-}

-- Recursión primitiva:
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

{-
Recursión iterativa:
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f ac [] = ac
foldl f ac (x : xs) = foldl f (f ac x) xs
-}

elem1 :: Eq a => a -> [a] -> Bool
elem1 e [] = False
elem1 e (x:xs) = x == e || elem1 e xs

-- usando foldr:
elem2 :: Eq a => a -> [a] -> Bool
elem2 e = foldr (\x rec -> x == e || rec) False

-- usando foldl:
elem3 :: Eq a => a -> [a] -> Bool
elem3 e = foldl (\acc x -> acc || x == e) False

-- usando recr (no conviene usarlo ya que en ningun momento se usa xs en la función):
elem4 :: Eq a => a -> [a] -> Bool
elem4 e = recr (\x xs acc -> x == e || acc) False

{-
sumaAlternada [1,2,3,4] --> 1 - 2 + 3 - 4 / output: -2
sumaAlternada [2,3,4] --> 2 - 3 + 4 / output: 3
sumaAlternada [] / output: 0
-}

sumaAlternada1 :: Num a => [a] -> a
sumaAlternada1 [] = 0
sumaAlternada1 (x:xs) = x - (sumaAlternada1 xs)

-- usando foldr:
sumaAlternada2 :: Num a => [a] -> a
sumaAlternada2 = foldr (-) 0 

-- usando foldr1 (no conviene usarla porque no contempla el caso de la lista vacia):
sumaAlternada3 :: Num a => [a] -> a
sumaAlternada3 = foldr1 (-)

-- para sumaAlternada el uso de foldl no conviene ya que al implementarse quedaria algo de este estilo (((0 - x1) - x2) - x3) = 0 - x1 - x2 - x3, donde nunca aparece un +. En cambio al usar foldr se obtiene algo del estilo x1 - (x2 - (x3 - 0)) = x1 - x2 + x3 - 0

-- la implementación de sumaAlternada usando recr es posible, pero al igual que con foldl, es mas complicada y menos conveniente que la que usa foldr.

take2 :: Int -> [a]-> [a]
take2 n [] = []
take2 n (x:xs) = if n == 0 then [] else x : take2 (n-1) xs

take3 :: [a] -> Int -> [a]
take3 [] n = []
take3 (x:xs) n = if n == 0 then [] else x : take3 xs (n-1)

take4 :: [a] -> Int -> [a]
take4 [] = const []
take4 (x:xs) = \n -> if n == 0 then [] else x : take4 xs (n-1)

-- usando foldr:
take5 :: [a] -> Int -> [a]
take5 = foldr (\x rec n -> if n == 0 then [] else x : rec (n - 1)) (const [])

take6 :: Int -> [a] -> [a]
take6 = flip take5

sacarUna1 :: Eq a => a -> [a] -> [a]
sacarUna1 e [] = []
sacarUna1 e (x:xs) = if e == x then xs else x : sacarUna1 e xs

sacarUna2 :: Eq a => a -> [a] -> [a]
sacarUna2 e = recr (\x xs rec -> if e == x then xs else x : rec) []

{-
g [] = z
g (x:xs) = f x (g xs)

x
xs
g

estructural --> x (g xs)
primitiva --> x (g xs) xs
global --> x xs g

Recursión Estructural: Recursión basada en la estructura de datos, donde la llamada recursiva opera sobre partes de la estructura.
Recursión Primitiva: Recursión basada en la reducción sistemática y directa de argumentos, comúnmente en estructuras como números naturales.
Recursión Global: Recursión que puede involucrar múltiples funciones recursivas y estados globales, con una estructura más compleja.
-}

take' :: [a] -> Int -> [a]
take' [] _ = []
take' (x:xs) n = if n == 0 then [] else x : take' xs (n-1)

-- take' es una función que usa recursión estructural (take' es igual a take3 de más arriba)

-- listasQueSuman es una función que utiliza recursión global
listasQueSuman :: (Num a, Enum a) => Int -> [[a]]
listasQueSuman 0 = [[]]
listasQueSuman n | n > 0 = [x : xs | x <- [1..], xs <- listasQueSuman (n - 1)]

-- fact es una función que usa recursión primitiva
fact :: Int -> Int
fact 0 = 1
fact n | n > 0 = n * fact (n - 1)

-- fibonacci es una función que usa recursión global
fibonacci :: Int -> Int
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n | n > 1 = fibonacci (n - 1) + fibonacci (n - 2)

pares :: [(Int, Int)]
pares = [(x, y) | s <- [0..], x <- [0..s], y <- [0..s], x + y == s]

{-
0,0
0,1
1,0
2,0
-}

-- Definición de un arbol binario:
data AEB a = Hoja a | Bin (AEB a) a (AEB a)
    deriving(Show)

-- Ejemplo de arbol binario para probar funciones
{- miArbolAEB:
    5
   / \
  3   8
     / \
    7   1
-}
miArbolAEB :: AEB Integer
miArbolAEB = Bin (Hoja 3) 5 (Bin (Hoja 7) 8 (Hoja 1))

foldAEB :: (a -> b) -> (b -> a -> b -> b) -> AEB a -> b
foldAEB fHoja fBin t = case t of
                       Hoja n -> fHoja n
                       Bin t1 n t2 -> fBin (rec t1) n (rec t2)
                       where rec = foldAEB fHoja fBin

alturaAEB :: AEB a -> Int
alturaAEB = foldAEB (const 1) (\izq _ der -> 1 + max izq der)

-- ramasAEB devuelve una lista con todas las ramas del arbol
ramasAEB :: AEB a -> [[a]]
ramasAEB = foldAEB (\n -> [[n]]) (\izq n der -> map (n :) (izq ++ der))

-- cantNodosAEB devuelve la cantidad total de nodos incluyendo los internos y las hojas
cantNodosAEB :: AEB a -> Int
cantNodosAEB = foldAEB (const 1) (\izq _ der -> 1 + izq + der)

cantHojasAEB :: AEB a -> Int
cantHojasAEB = foldAEB (const 1) (\izq _ der -> izq + der)

-- espejoAEB devuelve el arbol pero dado vuelta
-- ejemplo: print (espejoAEB miArbolAEB) / output: Bin (Bin (Hoja 1) 8 (Hoja 7)) 5 (Hoja 3)
espejoAEB :: AEB a -> AEB a
espejoAEB = foldAEB Hoja (\izq n der -> Bin der n izq)

-- hojasAEB devuelve la lista con todas las hojas
hojasAEB :: AEB a -> [a]
hojasAEB = foldAEB (: []) (\izq _ der -> izq ++ der)

-- nodosAEB devuelve la lista con todos los nodos
nodosAEB :: AEB a -> [a]
nodosAEB = foldAEB (: []) (\izq n der -> n : izq ++ der)


data AB a = Nil | Bin' (AB a) a (AB a)
    deriving(Show)

-- ejemplo de arbol AB
arbolAB :: AB Int
arbolAB = Bin' (Bin' Nil 1 Nil) 2 (Bin' Nil 3 Nil)

-- Recursión estructural de un arbol AB:
foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB cNil cBin Nil = cNil
foldAB cNil cBin (Bin' i r d) = cBin (foldAB cNil cBin i) r (foldAB cNil cBin d)

-- Recursión primitiva de un arbol AB:
recAB :: b -> (AB a -> b -> a -> AB a -> b -> b) -> AB a -> b
recAB cNil cBin Nil = cNil
recAB cNil cBin (Bin' i r d) = cBin i (recAB cNil cBin i) r d (recAB cNil cBin d)

-- La función insertarABB utiliza la recursión primitiva
insertarABB :: Ord a => a -> AB a -> AB a
insertarABB x Nil = Bin' Nil x Nil
insertarABB x (Bin' i r d) = if x < r
                            then Bin' (insertarABB x i) r d
                            else Bin' i r (insertarABB x d)

-- Versión de insertarABB con recAB:
insertarABB' :: Ord a => a -> AB a -> AB a
insertarABB' x = recAB (Bin' Nil x Nil) (\i ri r d rd -> if x < r then Bin' ri r d else Bin' i r rd)

-- La función truncar utiliza la recursión estructural
truncar :: AB a -> Int -> AB a
truncar Nil _ = Nil
truncar (Bin' i r d) n = if n == 0
                        then Nil
                        else Bin' (truncar i (n - 1)) r (truncar d (n - 1))

-- Versión de trucar usando foldAB:
truncar' :: AB a -> Int -> AB a
truncar' t n = foldAB (\_ -> Nil) (\i r d n -> if n == 0 then Nil else Bin' (i (n - 1)) r (d (n - 1))) t n


data Polinomio a = X
                 | Cte a
                 | Suma (Polinomio a) (Polinomio a)
                 | Prod (Polinomio a) (Polinomio a)

evaluar :: Num a => a -> Polinomio a -> a
evaluar x pol = case pol of
                X        -> x
                Cte c    -> c
                Suma p q -> evaluar x p + evaluar x q
                Prod p q -> evaluar x p * evaluar x q

foldPoli :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b 
foldPoli fX fCte fSuma fProd pol = case pol of 
                                    X        -> fX
                                    Cte c    -> fCte c
                                    Suma p q -> fSuma (rec p) (rec q)
                                    Prod p q -> fProd (rec p) (rec q)
    where rec = foldPoli fX fCte fSuma fProd

evaluar' :: Num a => a -> Polinomio a -> a
evaluar' x = foldPoli x id (+) (*)


data RoseTree a = Rose a [RoseTree a]
    deriving(Show)

-- ejemplo de RoseTree:
arbolRT = Rose 1 [Rose 2 [Rose 4 [], Rose 5 []], Rose 3 []]

foldRT :: (a -> [b] -> b) -> RoseTree a -> b 
foldRT fRose (Rose n hijos) = fRose n (map rec hijos)
    where rec = foldRT fRose

recRT :: (a -> [RoseTree a] -> [b] -> b) -> RoseTree a -> b
recRT fRose (Rose n hijos) = fRose n hijos (map rec hijos)
    where rec = recRT fRose

-- foldRT f = recRT (\n _ rec -> f n rec)

-- tamañoRT devuelve la cantidad total de nodos:
tamañoRT :: RoseTree a -> Int
tamañoRT = foldRT (\n rhijos -> 1 + sum rhijos)

hojasRT :: RoseTree a -> [a]
hojasRT = foldRT (\n rec -> if null rec then [n] else concat rec)

ramasRT :: RoseTree a -> [[a]]
ramasRT = foldRT (\n ns -> if null ns then [[n]] else map (n :) (concat ns))

alturaRT :: RoseTree a -> Int
alturaRT = foldRT (\_ rec -> if null rec then 1 else 1 + maximum rec)


type Conj a = (a -> Bool)

-- Conjunto vacio:
vacio :: Conj a
vacio = const False

agregar :: Eq a => a -> Conj a -> Conj a
agregar e c = \x -> x == e || c x

interseccion :: Conj a -> Conj a-> Conj a
interseccion c1 c2 = \e -> c1 e && c2 e

union :: Conj a -> Conj a-> Conj a
union c1 c2 = \e -> c1 e || c2 e

diferencia :: Conj a -> Conj a-> Conj a
diferencia c1 c2 = \e -> c1 e && not (c2 e)

complemento :: Conj a -> Conj a
complemento c1 = not.c1

-- Conjunto de números pares
conjPares :: Conj Int
conjPares x = x `mod` 2 == 0

-- Conjunto de números mayores que 5
mayoresQue5 :: Conj Int
mayoresQue5 x = x > 5

-- Agregar un número al conjunto vacío
singleton3 :: Conj Int
singleton3 = agregar 3 vacio

{-
Ejemplos de ejecución:
> conjPares 4
True

> mayoresQue5 4
False

> interseccion conjPares mayoresQue5 6
True

> union conjPares mayoresQue5 4
True

> diferencia conjPares mayoresQue5 6
False
-}
