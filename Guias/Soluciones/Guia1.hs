-- Ejercicio 1 (Probar Funciones!!!!)

-- max2 (x, y) | x >= y = x
--             | otherwise = y
-- max2 :: (Float, Float) -> Float
-- max2 NO está currificada!!
-- max2' :: Float -> Float -> Float
-- max2' x y | x >= y = x
--           | otherwise = y


-- normaVectorial (x, y) = sqrt (x^2 + y^2)
-- normaVectorial :: (Float, Float) -> Float
-- normaVectorial NO está currificada!!
-- normaVectorial' :: Float -> Float -> Float
-- normaVectorial' x y =  sqrt (x^2 + y^2)


-- subtract = flip (-)
-- subtract :: Float -> Float -> Float
-- subtract está currificada!!
-- El flip hace que el primer argumento sea el que se va a restar, es decir subtract a b = b - a


-- predecesor = subtract 1
-- predecesor :: Float -> Float
-- predecesor está currificada!!


-- evaluarEnCero = \f -> f 0
-- evaluarEnCero :: (Float -> b) -> b
-- evaluarEnCero està currificada ya que toma un único argumento!!


