-- Teórica  y Práctica 1

curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

-- map ya viene definida en el preludio
{-
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs
-}

-- map definida usando foldr:
mapFoldr1 :: (a -> b) -> [a] -> [b]
mapFoldr1 f = foldr ((:) . f) []

-- otra versión de map definida usando foldr:
mapFoldr2 :: (a -> b) -> [a] -> [b]
mapFoldr2 f = foldr (\x rec -> f x : rec) []

negativos :: [Float] -> [Float]
negativos [] = []
negativos (x:xs) | x < 0 = x : negativos xs
                 | otherwise = negativos xs

noVacias :: [[a]] -> [[a]]
noVacias [] = []
noVacias (x:xs) | length x > 0 = x : noVacias xs
                | otherwise = noVacias xs

-- filter ya viene definida en el preludio
{-
filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs) = if p x
                  then x : filter p xs
                  else filter p xs
-}

-- filter definido usando foldr:
filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr p = foldr (\x rec -> if p x then (:) x rec else rec) []

-- otra manera de definir filter con foldr:
filterFoldr' :: (a -> Bool) -> [a] -> [a]
filterFoldr' p = foldr (\x -> if p x then (:) x else id) [] 

predicadoPar :: Int -> Bool
predicadoPar x = x `mod` 2 == 0
predicadoMayorQueTres :: Int -> Bool
predicadoMayorQueTres x = x > 3

funcion :: [a -> Bool] -> [[a] -> [a]]
funcion = map filter

predicados :: [Int -> Bool]
predicados = [predicadoPar, predicadoMayorQueTres]

filtros :: [[Int] -> [Int]]
filtros = funcion predicados

listaDeNumeros :: [Int]
listaDeNumeros = [1, 2, 3, 4, 5, 6]

resultado :: [[Int]]
resultado = map (\filtro -> filtro listaDeNumeros) filtros
-- Resultado esperado: [[2, 4, 6], [4, 5, 6]]

sumaL :: [Int] -> Int
sumaL [] = 0
sumaL (x:xs) = x + sumaL xs

-- sumaL definida usando foldr:
sumaLFoldr :: [Int] -> Int
sumaLFoldr = foldr (+) 0

-- (++) ya está definido en el preludio
{-
(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)
-}

-- insertion sort
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insertar x (isort xs)

insertar :: Ord a => a -> [a] -> [a]
insertar x [] = [x]
insertar x (y:ys) | x <= y = x : y : ys
                  | otherwise = y : insertar x ys

-- insertion sort definido con un acumulador:
isortAc :: Ord a => [a] -> [a] -> [a]
isortAc ac [] = ac
isortAc ac (x:xs) = isortAc (insertar x ac) xs

-- selection sort
ssort :: Ord a => [a] -> [a]
ssort [] = []
ssort (x : xs) = minimo (x : xs) : ssort (sacarMinimo (x : xs))

minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)

-- definición usando if de mínimo:
minimo' :: Ord a => [a] -> a
minimo' (x:[]) = x
minimo' (x:xs) = if x < minimo' xs
                 then x
                 else minimo' xs

sacarMinimo :: Ord a => [a] -> [a]
sacarMinimo [] = []
sacarMinimo (x:xs) | x == m = xs
                   | otherwise = x : sacarMinimo xs
                    where m = minimo (x:xs)

-- foldr ya viene definido en el preludio
{-
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)
-}

producto :: [Int] -> Int
producto = foldr (*) 1

and, or :: [Bool] -> Bool
and = foldr (&&) True
or = foldr (||) False

-- reverse ya está definido en el preludio
{-
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]
-}

-- reverse definida usando foldr:
reverseFoldr1 :: [a] -> [a]
reverseFoldr1 = foldr (\x rec -> rec ++ [x]) []

reverseFoldr2 :: [a] -> [a]
reverseFoldr2 = foldr (flip (++) Prelude.. (: [])) []

-- reverse definida usando un acumulador:
reverseAc :: [a] -> [a] -> [a]
reverseAc ac [] = ac
reverseAc ac (x:xs) = reverseAc (x:ac) xs

-- reverse definido usando foldl:
reverseFoldl :: [a] -> [a]
reverseFoldl = foldl (flip (:)) []

lengthFoldr :: Foldable t => t a -> Int
lengthFoldr = foldr (const (+ 1)) 0

trim :: String -> String
trim [] = []
trim (x:xs) = if x == ' ' then trim xs else x:xs

-- trim definida usando foldr:
trimFoldr :: String -> String
trimFoldr = foldr (\x acc -> if null acc && x == ' ' then acc else x : acc) []

-- trim definida usando recr:
trimRecr :: String -> String
trimRecr = recr (\x xs rec -> if x == ' ' then rec else x : xs) []

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z [] = z
recr f z (x:xs) = f x xs (recr f z xs)

-- bin2dec pasa de binario a decimal
bin2dec :: Int -> [Int] -> Int
bin2dec ac [] = ac
bin2dec ac (b:bs) = bin2dec (b + 2 * ac) bs

-- bin2dec definida usando foldl:
bin2decFoldl :: [Int] -> Int
bin2decFoldl = foldl (\ac b -> b + 2 * ac) 0

-- foldl ya viene definida en el preludio
{-
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f ac [] = ac
foldl f ac (x : xs) = foldl f (f ac x) xs
-}

data Dia = Dom | Lun | Mar | Mie | Jue | Vie | Sab

esFinDeSemana :: Dia -> Bool
esFinDeSemana Sab = True
esFinDeSemana Dom = True
esFinDeSemana _ = False

-- LaPersona es el nombre del donstructor de Persona
data Persona = LaPersona String String Int

nombre, apellido :: Persona -> String
fechaNacimiento :: Persona -> Int
nombre (LaPersona n _ _) = n
apellido (LaPersona _ a _) = a
fechaNacimiento (LaPersona _ _ f) = f

data Forma = Rectangulo Float Float | Circulo Float

area :: Forma -> Float
area (Rectangulo ancho alto) = ancho * alto
area (Circulo radio) = radio * radio * pi

data Nat = Zero | Succ Nat

dobleNat :: Nat -> Nat
dobleNat Zero = Zero
dobleNat (Succ n) = Succ (Succ (dobleNat n))

type Cuenta = String
data Banco = Iniciar
            | Depositar Cuenta Int Banco
            | Extraer Cuenta Int Banco
            | Transferir Cuenta Cuenta Int Banco

bancoPLP = Transferir "A" "B" 3 (Depositar "A" 10 Iniciar)

saldo :: Cuenta -> Banco -> Int
saldo cuenta Iniciar = 0
saldo cuenta (Depositar cuenta' monto banco) | cuenta == cuenta' = saldo cuenta banco + monto
                                             | otherwise = saldo cuenta banco
saldo cuenta (Extraer cuenta' monto banco) | cuenta == cuenta' = saldo cuenta banco - monto
                                           | otherwise = saldo cuenta banco
saldo cuenta (Transferir origen destino monto banco) | cuenta == origen = saldo cuenta banco - monto
                                                     | cuenta == destino = saldo cuenta banco + monto
                                                     | otherwise = saldo cuenta banco

-- definición de un arbol binario:
data AB a = Nil | Bin (AB a) a (AB a)

arbolPrueba = Bin (Bin (Bin Nil 3 Nil) 2 (Bin Nil 4 Nil)) 1 (Bin (Bin Nil 6 Nil) 5 (Bin Nil 7 Nil))

preorder :: AB a -> [a]
preorder Nil = []
preorder (Bin izq val der) = [val] ++ preorder izq ++ preorder der
-- preorder arbolPrueba / output: [1, 2, 3, 4, 5, 6, 7]

postorder :: AB a -> [a]
postorder Nil = []
postorder (Bin izq val der) = postorder izq ++ postorder der ++ [val]
-- postorder arbolPrueba / output: [3, 4, 2, 6, 7, 5, 1]

inorder :: AB a -> [a]
inorder Nil = []
inorder (Bin izq val der) = inorder izq ++ [val] ++ inorder der
-- inorder arbolPrueba / output: [3, 2, 4, 1, 6, 5, 7]

insertarAB :: Ord a => a -> AB a -> AB a
insertarAB x Nil = Bin Nil x Nil
insertarAB x (Bin izq y der) | x < y = Bin (insertarAB x izq) y der
                             | x > y = Bin izq y (insertarAB x der)
                             | otherwise = Bin izq y der


foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB cNil cBin Nil = cNil
foldAB cNil cBin (Bin i r d) = cBin (foldAB cNil cBin i) r (foldAB cNil cBin d)

-- función identidad para un arbol binario definida con foldAB:
idAB :: AB a -> AB a
idAB = foldAB Nil Bin

mapAB :: (a -> b) -> AB a -> AB b
mapAB f = foldAB Nil (\i r d -> Bin i (f r) d)

-- length ya viene definida en el preludio
{-
length :: [a] -> Int
length [] = 0
length (_ : xs) = 1 + length xs
-}

head :: [a] -> a
head [] = error "No tiene cabeza."
head (x : _) = x

-- versión currificada de prod':
prod :: Int -> (Int -> Int)
prod x y = x * y

-- versión no currificada de prod:
prod' :: (Int, Int) -> Int
prod' (x, y) = x * y

-- versión de prod escrita con una función lambda:
prodLambda :: Int -> Int -> Int
prodLambda x = \y -> x*y

doble :: Int -> Int
doble x = prod 2 x

doble' :: Int -> Int
doble' = prod 2

triple :: Float -> Float
triple = (*) 3.0

esMayorDeEdad :: Int -> Bool
esMayorDeEdad = (<=) 18

-- (.) ya viene definida en el preludio
{-
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g x = f (g x)
-}

-- flip ya viene definida en el preludio
{-
flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x
-}

($) :: (a -> b) -> a -> b
f $ x = f x

-- const ya viene definida en el preludio
{-
const :: a -> b -> a
const x _ = x
-}

-- const definida usando una función lambda:
constLambda :: a -> b -> a 
constLambda x = (\y -> x)

lista1 = [3..7]         -- output: [3, 4, 5, 6, 7]
lista2 = [2, 5..18]     -- output: [2, 5, 8, 11, 14, 17]
lista3 = [(x,y) | x <- [0..5], y <- [0..3], x+y==4]     -- output: [(1,3),(2,2),(3,1),(4,0)]
naturales = [1..]       -- el output es una lista infinita de números enteros a partir del 1
multiplosDe3 = [0,3..]  -- el output es una lista infinita que contiene los múltiplos de 3 a partir de 0
lista4 = repeat "hola"  -- el output es una lista infinita donde se repite la palabra hola
primos = [n | n <- [2..], esPrimo n]    -- el output es una lista infinita de números primos
infinitosUnos = 1 : infinitosUnos   -- el output es una lista infinita de unos

-- la función all verifica si todos los elementos de una lista cumplen una condición dada
esPrimo :: Int -> Bool
esPrimo n = all (\x -> n `mod` x /= 0) [2..(n-1)]

-- take ya viene definida en el preludio
{-
take :: Int -> [a] -> [a]
take 0 = []
take [] = []
take n (x:xs) = x : take (n-1) xs
-}

nUnos :: Int -> [Int]
nUnos n = take n infinitosUnos
-- nUnos 2 / output: [1, 1]

maximo :: Ord a => [a] -> a
maximo (x:[]) = x
maximo (x:xs) = if x > maximo xs
                then x
                else maximo xs

-- maximo definida usando la función mejorSegun:
maximo' :: Ord a => [a] -> a
maximo' = mejorSegun (>)

listaMasCorta :: [[a]] -> [a]
listaMasCorta (x:[]) = x
listaMasCorta (x:xs) = if length x < length (listaMasCorta xs)
                       then x
                       else listaMasCorta xs

-- listaMasCorta definida usando mejorSegun:
listaMasCorta' :: [[a]] -> [a]
listaMasCorta' = mejorSegun (\x y -> length x < length y)

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f [x] = x
mejorSegun f (x:y:xs) = if f x y
                        then mejorSegun f (x:xs)
                        else mejorSegun f (y:xs)

-- definición alternativa de mejorSegun:
mejorSegun' :: (a -> a -> Bool) -> [a] -> a
mejorSegun' f [x] = x
mejorSegun' f (x:xs) | f x (mejorSegun' f xs) = x
                     | otherwise = mejorSegun' f xs

mejorSegunFoldr1 :: (a -> a -> Bool) -> [a] -> a
mejorSegunFoldr1 f = foldr1 (\x y -> if f x y then x else y)

mejorSegunFoldl1 :: (a -> a -> Bool) -> [a] -> a
mejorSegunFoldl1 f = foldl1 (\x y -> if f x y then x else y)

deLongitudN :: Int -> [[a]] -> [[a]]
deLongitudN n = filter (\x -> length x == n)

-- otra manera de escribir deLongitudN:
deLongitudN' :: Int -> [[a]] -> [[a]]
deLongitudN' n = filter ((== n) . length)

soloPuntosFijosEnN :: Int -> [Int->Int] -> [Int->Int]
soloPuntosFijosEnN n = filter (\f -> f n == n)

reverseAnidado :: [[Char]] -> [[Char]]
reverseAnidado = reverse . map reverse

paresCuadrados :: [Int] -> [Int]
paresCuadrados = map (\x -> if even x then x * x else x)

listaComp :: (a -> b) -> [a] -> (a -> Bool) -> [b]
listaComp f xs p = [f x | x <- xs, p x]

-- listaComp definida usando map y filter:
listaComp' :: (a -> b) -> [a] -> (a -> Bool) -> [b]
listaComp' f xs p = map f (filter p xs)