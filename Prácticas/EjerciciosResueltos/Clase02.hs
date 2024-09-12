-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f z [] = z
-- foldr f z (x : xs) = f x (foldr f z xs)

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl f ac [] = ac
-- foldl f ac (x : xs) = foldl f (f ac x) xs


elem2 :: Eq a => a -> [a] -> Bool
elem2 e []     = False
elem2 e (x:xs) = x == e || elem2 e xs

elem3 e = foldr (\x rec -> x == e || rec) False


{-
sumaAlternada [1,2,3,4] --> 1 - 2 + 3 - 4

sumaAlternada [2,3,4] --> 2 - 3 + 4

f 1 (2 - 3 + 4) --> 1 - 2 + 3 - 4

                    1 - (2 - 3 + 4)

sumaAlternada [] = 0
sumaAlternada (x:xs) = x - (sumaAlternada xs)

sumaAlternada = foldr (\x rec -> x - rec) 0 

[1,2,3,4]
--> 1 - sA [2,3,4]
        2  - sA [3,4]
              3 - sA [4]
                   ...
                   4

              -1
        3
    -2



-}
sumaAlternada :: Num a => [a] -> a
sumaAlternada = foldr (-) 0 
-- sumaAlternada = foldr1 (-)



take2 :: Int -> [a]-> [a]
take2 n []     = []
take2 n (x:xs) = if n == 0 then [] else x : take2 (n-1) xs


take3 :: [a] -> (Int -> [a])
take3  []     n = []
take3  (x:xs) n = if n == 0 then [] else x : take3 xs (n-1)

take4 :: [a] -> (Int -> [a])
take4  []     = \n -> [] -- const []
take4  (x:xs) = \n -> if n == 0 then [] else x : take4 xs (n-1)


take5 :: [a] -> (Int -> [a])
take5 = foldr (\x rec -> \n -> if n == 0 then [] else x : rec (n-1)) 
              (\n -> [])


take6 :: Int -> [a] -> [a]
take6 = flip take5

--Pensar take con foldNat




sacarUna:: Eq a => a -> [a] -> [a]
sacarUna e []     = []
sacarUna e (x:xs) = if e == x then xs else x : sacarUna e xs

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
-}


pares :: [(Int, Int)]
pares = [(x,y) | s <- [0..], x <- [0..s], y <- [0..s], x + y == s]

{-
0,0
0,1
1,0
2,0
-}



data AEB a = Hoja a | Bin (AEB a) a (AEB a)


foldAEB :: (a -> b -> b -> b) -> (a -> b) -> AEB a -> b
foldAEB fBin fHoja t = case t of
                            Hoja h    -> fHoja h
                            Bin i r d -> fBin r (rec i) (rec d)
    where rec = foldAEB fBin fHoja
    

hojas :: AEB a -> [a]
hojas = foldAEB (\r ri rd -> ri ++ rd) (\h -> [h])

nodos :: AEB a -> [a]
nodos = foldAEB (\r ri rd -> r : ri ++ rd) (\h -> [h])


miArbol = Bin (Hoja 3) 5 (Bin (Hoja 7) 8 (Hoja 1))



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

foldPoli :: (b) -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b 
foldPoli fX fCte fSuma fProd pol = case pol of 
                                        X        -> fX
                                        Cte c    -> fCte c
                                        Suma p q -> fSuma (rec p) (rec q)
                                        Prod p q -> fProd (rec p) (rec q)
    where rec = foldPoli fX fCte fSuma fProd


evaluar2 :: Num a => a -> Polinomio a -> a
evaluar2 x = foldPoli x id (+) (*)






data RoseTree a = Rose a [RoseTree a]

foldRT :: (a -> [b] -> b) -> RoseTree a -> b 
foldRT fRose (Rose n hijos) = fRose n (map rec hijos)
    where rec = foldRT fRose

recRT :: (a -> [RoseTree a] -> [b] -> b) -> RoseTree a -> b
recRT fRose (Rose n hijos) = fRose n hijos (map rec hijos)
    where rec = recRT fRose

-- foldRT f = recRT (\n _ rec -> f n rec)


cantNodos :: RoseTree a -> Int
cantNodos = foldRT (\n rhijos -> 1 + sum rhijos)

hojasRT :: RoseTree a -> [a]
hojasRT = foldRT (\n rec -> if null rec then [n] else concat rec)

altura :: RoseTree a -> Int
altura = foldRT (\_ rec -> if null rec then 0 else 1 + maximum rec)


miRT = Rose 1 [Rose 2 [Rose 3 [], Rose 4 [Rose 5 [], Rose 6 [], Rose 7 []]]]

{-
[[[4,5], [4,6], [4,7]],[[3]]]
-}

type Conj a = (a->Bool)

vacio :: Conj a

solo1 = \e -> if e == 1 then True else False

vacio = const False

agregar :: Eq a => a -> Conj a -> Conj a
agregar e c = \e2 -> e2 == e || c e2

udt = agregar 1 (agregar 2 (agregar 3 vacio))

interseccion :: Conj a -> Conj a-> Conj a
interseccion c1 c2 = \e -> c1 e && c2 e

union :: Conj a -> Conj a-> Conj a
union c1 c2 = \e -> c1 e || c2 e

diferencia :: Conj a -> Conj a-> Conj a
diferencia c1 c2 = \e -> c1 e && not (c2 e)

complemento :: Conj a -> Conj a
complemento c1 = not.c1

{-
{1,2,3}

Ag(1, Ag(2, Ag(3, {})))
-}












