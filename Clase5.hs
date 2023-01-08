module Clase5
where

esPar :: Integer -> Bool
esPar n 
		| n == 0    = True
		| otherwise = esImpar(abs n - 1)

esImpar :: Integer -> Bool
esImpar n 
		 | n == 0    = False
		 | otherwise = esPar(abs n - 1) 


sumaLosNPrimerosImpares :: Integer -> Integer
sumaLosNPrimerosImpares n 
						  | n == 1 = 1
						  | n > 1  = sumaLosNPrimerosImpares (n-1) + n_esimo_impar
						  where n_esimo_impar = 2*n - 1

-- def sumaLosNPrimerosImpares(n):
--   suma = 0
--   cont = 0
--   i = 1
--   while cont < n:
--     if i % 2:
--       suma += i
--       cont += 1
--     i += 1
--   return suma

factorial :: Integer -> Integer
factorial n 
			| n == 0 = 1
			| otherwise = factorial (n-1) * n

eAprox :: Integer -> Float
eAprox n 
         | n == 0 = 1
         | n >= 1 = eAprox (n-1) + 1 / (fromIntegral (factorial n))

e :: Float 
e = eAprox 10 

-- def factorial(n):
--   return 1 if n == 0 else factorial(n-1)*n

-- def eAprox(n):
--   eAprox = 0
--   for i in range(0,n+1):
--     eAprox += 1 / factorial(i)
--   return eAprox

-- Implementar una función parteEntera :: Float-> Integer
-- que calcule la parte entera de un número real.

parteEntera :: Float -> Integer
parteEntera x 
		      | x >= 0   = piso x
		      |otherwise = techo x

piso :: Float -> Integer
piso x = round x

techo :: Float -> Integer
techo x = round x + 1

division :: Integer -> Integer -> (Integer,Integer)
division a d 
			| d > a = (0,a)
			| otherwise = 
				(fst((division (a-d) d)) + 1, snd(division (a-d) d))
			
-- sumaDivisores :: Integer-> Integer que calcule la
-- suma de los divisores un número.

sumaDivisoresDesde :: Integer -> Integer -> Integer
sumaDivisoresDesde n d 
					  | n < d           = 0
					  | esDivisible n d = d + sumaDivisoresDesde n (d+1)
					  | otherwise       = sumaDivisoresDesde n (d+1)
					  where esDivisible n d = mod n d == 0


sumaDivisores :: Integer -> Integer
sumaDivisores n = sumaDivisoresDesde n 1 

-- def sumaDivisores(n):
--   suma = 0
--   for num in range(1,(n//2) + 1):
--     if not n % num:
--       suma += num
--   return suma + n

-- Un entero p > 1 es primo si ningún natural k 
-- tal que 1 < k < p divide a p.

tieneDivisoresPropios :: Integer -> Bool
tieneDivisoresPropios n = tieneDivisoresPropiosDesde n 2

tieneDivisoresPropiosDesde :: Integer -> Integer -> Bool
tieneDivisoresPropiosDesde n k
							   | n == k          = False 
							   | esDivisible n k = True
							   | otherwise       = tieneDivisoresPropiosDesde n (k+1)
							   where esDivisible n k = mod n k == 0

esPrimo :: Integer -> Bool
esPrimo n = (n > 1) && not (tieneDivisoresPropios n)

-- menorDivisor :: Integer-> Integer que calcule el menor 
-- divisor (mayor que 1) de un natural n.

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n d
					  | esDivisible n d = d
					  | otherwise       = menorDivisorDesde n (d+1)
					  where esDivisible n d = mod n d == 0

menorDivisor :: Integer -> Integer 
menorDivisor n = menorDivisorDesde n 2

esPrimo' :: Integer -> Bool
esPrimo' n = (n > 1) && menorDivisor n == n

--------------------------------------------------------------------------------------------------------
f1 ::  Integer -> Integer -> Integer
f1 n m 
	   | m == 0    = 0
	   | otherwise = f1 n (m-1) + n^m

f2 :: Float -> Integer -> Float
f2 q n 
	  | n == 0    = 0
	  | otherwise = f2 q (n-1) + q^n

f :: Integer -> Integer -> Integer
f n m
	 | n == 0 = 0
	 | otherwise = f (n-1) m + f1 n m 


-- Implementar una función sumaPotencias q n m que sume todas las potencias
-- de la forma q^(a+b) con 1 ≤ a ≤ n y 1≤ b ≤m.

sumaPotencias :: Float -> Integer -> Integer -> Float
sumaPotencias q n m
					| n == 0    = 0
					| otherwise = sumaPotencias q (n-1) m + (q^n)*(f2 q m)

-- Implementar una función sumaRacionales n m que sume todos los números racionales
-- de la forma p/q con 1 ≤ p ≤ n y 1 ≤ q ≤ m.

sumaRacionales :: Integer -> Integer -> Float
sumaRacionales n m
				  | m == 0    = 0
				  | otherwise = sumaRacionales n (m-1) + fromIntegral((sumatoria n)) / (fromIntegral m)
				  -- where sumatoria n = if n == 0 then 0 else sumatoria (n-1) + n

sumatoria :: Integer -> Integer
sumatoria n
		   | n == 0    = 0
		   | otherwise = sumatoria (n-1) + n


