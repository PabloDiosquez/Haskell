module Clase6
where

import Clase5 hiding (factorial)

-- Si quisiéramos definir la función negLogica (negación lógica), podríamos hacerlo así:

negLogica :: Bool -> Bool
negLogica x 
			| x == True = False
			| otherwise = True

negLogica' :: Bool -> Bool
negLogica' True = False
negLogica' False = True

-- Reescribir la función factorial:: Integer -> Integer usando Pattern Matching.

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = factorial (n-1) * n 

-- Escribir las definiciones de las siguientes funciones, utilizando pattern matching.
-- Tratar de evaluar la mínima cantidad de parámetros necesaria.
-- (a) yLogico :: Bool-> Bool-> Bool, la conjunción lógica.

yLogico :: Bool -> Bool -> Bool
yLogico True True = True
yLogico _ _       = False

-- (b) oLogico :: Bool-> Bool-> Bool, la disyunción lógica.

oLogico :: Bool -> Bool -> Bool
oLogico False False = False
oLogico _ _         = True

-- (c) implica :: Bool-> Bool-> Bool, la implicación lógica.

implica :: Bool -> Bool -> Bool 
implica False True = False
implica _ _        = True

-- (d) sumaGaussiana :: Integer-> Integer, que toma un entero no negativo y devuelve la suma de todos
-- los enteros positivos menores o iguales que él.

sumaGaussiana :: Integer -> Integer
sumaGaussiana 0 = 0 
sumaGaussiana n = sumaGaussiana (n-1) + n 

-- (e) algunoEsCero :: (Integer, Integer, Integer) -> Bool, que devuelve True sii alguna de las componentes
-- de la tupla es 0.

algunoEsCero :: (Integer,Integer,Integer) -> Bool
algunoEsCero (0,_,_) = True
algunoEsCero (_,0,_) = True
algunoEsCero (_,_,0) = True
algunoEsCero (n,m,t) = False

-- (f) productoInterno :: (Float, Float)-> (Float, Float)-> Float, que dados dos vectores
-- v1 = (x1,y1), v2 = (x2,y2) ∈ R2, calcula su producto interno v1, v2= x1x2 +y1y2.

productoInterno :: (Float,Float) -> (Float,Float) -> Float
productoInterno (x1,y1) (x2,y2) = x1*x2 + y1*y2

-- Implementar una función que, dado un número natural n, determine si puede escribirse como
-- suma de dos números primos: esSumaDeDosPrimos :: Integer-> Bool

esSumaDeDosPrimos :: Integer -> Bool
esSumaDeDosPrimos n = esSumaDeDosPrimosAux n 2

esSumaDeDosPrimosAux :: Integer -> Integer -> Bool
esSumaDeDosPrimosAux n a 
						 | a == n                     = False
						 | esPrimo a && esPrimo(n-a) = True
						 | otherwise                 = esSumaDeDosPrimosAux n (a+1)


-- Conjetura (Christian Goldbach, 1742): todo número par mayor que 2 puede escribirse como suma de dos
-- números primos. Escribir una función que pruebe la conjetura hasta un cierto punto.
-- goldbach :: Integer -> Bool (hasta al menos 4.1018 debería ser cierto)

goldbach :: Integer -> Bool
goldbach 4 = True
goldbach n
		   | esPar n   = (esSumaDeDosPrimos n) && goldbach (n-2)
		   | otherwise = goldbach (n-1)
		   where esPar n = mod n 2 == 0

-- Escribir una función que determine la suma de dígitos de un número positivo.
-- Para esta función pueden utilizar div y mod.

-- n = d1d2...dk
-- sumaDigitos n = sumaDigitos (d1d2...d(k-1)) + dk

sumaDigitos :: Integer -> Integer
sumaDigitos n | n < 10 = n
sumaDigitos n = sumaDigitos (div n 10) + ultimoDigito
			  where ultimoDigito = mod n 10

-- Implementar una función que determine si todos los dígitos de un número son iguales.

digitosIguales :: Integer -> Bool
digitosIguales n = if n <= 9 
					then True
					else (mod n 10) == (mod (div n 10) 10) && digitosIguales (div n 10)

-- Sea la siguiente definición:
-- an+1 | an*2, si an es par
-- 		| 3an + 1, si an es impar

-- Si comenzamos con a1 = 13, obtenemos la siguiente secuencia: 13 →40→20→10→5→16→8→4→2→1(10 términos).
-- No fue demostrado aún (conjetura de Collatz), que empezando con cualquier número siempre se termina en 1.