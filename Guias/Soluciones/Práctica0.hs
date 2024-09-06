-- Para empezar a usar haskell en la terminal ingresar ghci
-- Para preguntarle a la terminal un tipo de dato hat que escribir primero :t
-- Para cargar un archivo usar :l nombreDelArchivoSinExtensión
-- Para hacer refresh a un archivo usar :r
-- Para salir del ghci utilizar Ctrl D

-- Ejercicio 1

-- null xs verifica si xs es la lista vacía
  -- null [] / output: True
  -- null [4] / output: False
-- null :: Foldable t => t a -> Bool
-- null :: [a] -> Bool

-- head xs devuelve el primer elemento de la lista xs
  -- head [4, 3, 5] / output: 4
  -- head "Hola" / output: 'H'
-- head :: [a] -> a

-- tail xs devuelve la lista obtenida eliminando el primer elemento de xs
  -- tail [4, 5, 3] / output: [5, 3]
  -- tail [3] / output: []
  -- tail "Hola" / output: "ola"
-- tail :: [a] -> [a]

-- init xs devuelve la lista obtenida eliminando el último elemento de xs
  -- init [4, 5, 3] / output: [4, 5]
  -- init [3] / output: []
  -- init "Hola" / output: "Hol"
-- init :: [a] -> [a]

-- last xs devuelve el último elemento de la lista xs
  -- last [4, 5, 3] / output: 3
  -- last [3] / output: 3
  -- last "Hola" / output: 'a'
-- last :: [a] -> a

-- take n xs devuelve la lista de los n primeros elementos de xs
  -- take 5 [1..] / output: [1, 2, 3, 4, 5]
  -- take 1 [4, 5, 3] / output: [4]
  -- take 2 "Hola" / output: "Ho"
-- take :: Int -> [a] -> [a]

-- drop n xs borra los n primeros elementos de xs
  -- drop 1 [4, 5, 3] / output: [5, 3]
  -- drop 2 "Hola" / output: "la"
  -- drop 5 [3] /  output: []
-- drop :: Int -> [a] -> [a]

-- xs ++ ys es la concatenación de xs e ys
  -- [4] ++ [5, 3] / output: [4, 5, 3]
  -- "Ho" ++ "la" / output: "Hola"
-- (++) :: [a] -> [a] -> [a]

-- concat xss es la concatenación de la lista de listas xss
  -- concat [[2,5],[7],[4,9,6]] / output: [2,5,7,4,9,6]
  -- concat ["Sal","a","manca"] / output: "Salamanca"
-- concat :: Foldable t => t [a] -> [a]
-- concat :: [[a]] -> [a]

-- reverse xs es la inversa de la lista xs
  -- reverse [3,5,2,4] / output: [4,2,5,3]
  -- reverse "Hola" / output: "aloH"
-- reverse :: [a] -> [a]

-- elem x ys se verifica si x pertenece a ys
  -- elem 3 [4, 5, 3] / output: True
  -- elem 9 [4, 5, 3] / output: False
  -- elem 'o' "Hola" / output: True
-- elem :: (Foldable t, Eq a) => a -> t a -> Bool
-- elem :: a -> [a] -> Bool

-- Ejercicio 2

-- a)
valorAbsoluto :: Float -> Float
valorAbsoluto n | n >= 0 = n
                | otherwise = (-n)

-- b)
bisiesto :: Int -> Bool
bisiesto n = ((mod n 4 == 0) && (mod n 100 /= 0)) || (mod n 400 == 0)

-- c)
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- d)
cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos n = contadorDivisoresPrimos n n

contadorDivisoresPrimos :: Int -> Int -> Int
contadorDivisoresPrimos _ 1 = 0
contadorDivisoresPrimos n d | ((contadorDivisores d d) == 2) && (mod n d == 0) = 1 + contadorDivisoresPrimos n (d - 1)
                            | otherwise = contadorDivisoresPrimos n (d - 1)

contadorDivisores :: Int -> Int -> Int
contadorDivisores _ 1 = 1
contadorDivisores n d | mod n d == 0 = 1 + contadorDivisores n (d - 1)
                      | otherwise = contadorDivisores n (d - 1)

-- Ejercicio 3

-- a)
inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso n = Just (1 / n)

-- b)
aEntero :: Either Int Bool -> Int
aEntero n = case n of
      (Left a) -> a
      (Right b) -> if b then 1 else 0

-- Esta función se ejecuta como:
  -- aEntero (Left 42) / output: 42
  -- aEntero (Right True) / output: True

-- Ejercicio 4

-- a)
limpiar :: String -> String -> String
limpiar [] ys = ys
limpiar _ [] = []
limpiar (x:xs) ys = limpiar xs (eliminarChar x ys)

eliminarChar :: Char -> String -> String
eliminarChar _ [] = []
eliminarChar c (y:ys) | c == y = eliminarChar c ys
                      | otherwise = y : eliminarChar c ys

-- b) PROBAR SI FUNCIONA!!!
difPromedio :: [Float] -> [Float]
difPromedio [] = []
difPromedio (x:xs) = (x - promedio (x:xs)) : difPromedio xs

sumatoria :: [Float] -> Float
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

longitud :: [Float] -> Float
longitud [] = 0
longitud (x:xs) = 1 + sumatoria xs

promedio :: [Float] -> Float
promedio [] = 0
promedio (x:xs) = sumatoria (x:xs) / longitud (x:xs)

-- c) PROBAR SI FUNCIONA!!!
todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales (y:[]) = True
todosIguales (x:y:ys) | if x == y then todosIguales (y:ys) else False

-- Ejercicio 5
-- Modelo para àrboles binarios: AB a = Nil | Bin (AB a) a Bin (AB a)

-- a) PROBAR SI FUNCIONA!!!
vacioAB :: AB a -> Bool
vacioAB a = if a == Nil then True else False

-- b) PROBAR SI FUNCIONA!!!
negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin izq valor der) = Bin (negacionAB izq) (not a) Bin(negacionAB der)

-- c) PROBAR SI FUNCIONA!!!
productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin izq valor der) = productoAB izq * valor * productoAB der
