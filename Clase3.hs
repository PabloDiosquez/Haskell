module Clase3
where

import Clase2 (esMultiploDe)

-- unidades: dado un entero, devuelve el dígito de las unidades del número (el dígito menos significativo).

unidades :: Int -> Int 
unidades n = mod n 10

-- sumaUnidades3: dados 3 enteros, devuelve la suma de los dígitos de las unidades de los 3 números.

sumaUnidades3 :: Int -> Int -> Int -> Int
sumaUnidades3 n m t = unidades n + unidades m + unidades t 

-- todosImpares: dados 3 números enteros determina si son todos impares.

todosImpares :: Int -> Int -> Int -> Bool 
todosImpares n m t = (esImpar n) && (esImpar m) && (esImpar t)
					where esImpar n = mod n 2 == 1

-- alMenosUnImpar: dados 3 números enteros determina si al menos uno de ellos es impar.

alMenosUnImpar :: Int -> Int -> Int -> Bool
alMenosUnImpar n m t = esImpar n || esImpar m || esImpar t
					 where esImpar n = mod n 2 == 1  

-- alMenosDosImpares: dados 3 números enteros determina si al menos dos de ellos son impares.

-- alMenosDosImpares :: Int -> Int -> Int -> Bool
-- alMenosDosImpares n m t | esImpar n && esImpar m = True
-- 						| esImpar n && esImpar t = True
-- 						| esImpar m && esImpar t = True
-- 						| otherwise              = False
-- 						where esImpar n = mod n 2 == 1 


alMenosDosImpares :: Int -> Int -> Int -> Bool 
alMenosDosImpares n m t = (esImpar n && esImpar m) || (esImpar n && esImpar t) || (esImpar m && esImpar t) 
						where esImpar n = mod n 2 == 1

-- alMenosUnMultiploDe: dados 3 números enteros determina si alguno de los primeros dos es múltiplo del tercero.

alMenosUnMultiploDe:: Int -> Int -> Int -> Bool
alMenosUnMultiploDe n m t = esMultiploDe n t || esMultiploDe m t 

-- Dados dos enteros a, b implementar funciones:
-- (r1, r2 y r3) :: Integer-> Integer-> Bool que determinen si a ∼ b donde: 
-- 1) a∼b si tienen la misma paridad
-- 2) a∼b si 2a+3b es divisible por 5 
-- 3) a∼b si los dígitos de las unidades de a, b y a · b son todos distintos

-- r1 :: Integer -> Integer -> Bool
-- r1 a b = (mod a 2) == (mod b 2)

r1 :: Integer -> Integer -> Bool
r1 a b = if esPar a && esPar b 
			then True 
			else if not (esPar a) && not (esPar b) 
				    then True
				    else False
		where esPar a = mod a 2 == 0 

r2 :: Integer -> Integer -> Bool
r2 a b = mod (2*a + 3*b) 5 == 0

unidadesDistintas :: Integer -> Integer -> Bool
unidadesDistintas a b = (mod a 10) /= (mod b 10)

r3 :: Integer -> Integer -> Bool
r3 a b = unidadesDistintas a b && unidadesDistintas a (a*b) && unidadesDistintas b (a*b)   

-- Se define en R la relación de equivalencia asociada a la partición R = (−∞,3)∪[3,+∞).
-- Determinar el tipo e implementar una función que dados dos números x,y ∈ R determine si x∼y.

r :: Float -> Float -> Bool
r x y | x < 3 && y < 3   = True
	  | x >= 3 && y >= 3 = True
	  | otherwise        = False

-- Repetir el ejercicio anterior para la partición R =(−∞,3)∪[3,7)∪[7,+∞).

r' :: Float -> Float -> Bool
r' x y | x < 3 && y < 3                         = True
       | (x >= 3 && x < 7) && (y >= 3 && y < 7) = True
       | x >= 7 && y >= 7                       = True
       | otherwise                              = False


-- Definir una función para calcular el factorial de un número.

factorial :: Integer -> Integer
factorial n 
			| n == 0 = 1
			| n > 0 = n * factorial(n-1)

-- Ejercicios

sc :: Integer -> Integer
sc n 
	| n == 0 = 0
	| otherwise = sc(n-1) + n^2

-- Implementar la función fib :: Integer -> Integer que devuelve el i-ésimo Fibonacci.

fib :: Integer -> Integer
fib n
	 | n == 0 = 0
	 | n == 1 = 1
	 | n > 1  = fib(n-1) + fib(n-2)

a1 :: Integer -> Integer
a1 n 
    | n == 1 = 2
    | n > 1  = 2*n*(a1(n-1)) + 2^(n+1) * (factorial n) 