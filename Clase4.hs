module 	Clase4 
where 

factorial :: Integer ->Integer
factorial n 
			| n == 0 = 1
			| otherwise = factorial (n-1) * n 

-- Implementar la función fib :: Integer-> Integer
-- que devuelve el i-ésimo número de Fibonacci.		

fib :: Integer -> Integer
fib n
	 | n == 0 = 0
	 | n == 1 = 1 
	 | n > 1  = fib (n-1) + fib (n-2) 

-- def fib(num):
--   if num == 0:
--     fibo = 0
--   else:
--     if num == 1:
--       fibo = 1
--     else:
--       fibo = fib(num - 1) + fib(num - 2)
--   return fibo 

-- def fib(num):
--   fib0 = 0
--   fib1 = 1
--   for i in range(2,num+1):
--     fibo = fib0 + fib1
--     fib0 = fib1
--     fib1 = fibo
--   return fibo 

a :: Integer -> Integer
a n 
	| n == 1 = -3
	| n == 2 = 6
	| otherwise = if esPar n 
					then a(n-1) + 2*a(n-2) + 9
					else -(a(n-1)) - 3
	where esPar n = mod n 2 == 0

-- Implentar una función que calcule la sumatoria de los n primeros números naturales.

sumatoria :: Integer -> Integer
sumatoria n 
			| n == 0 = 0
			| n >= 1 = sumatoria (n-1) + n 

-- def sumatoria(n):
--   sumatoria = 0
--   for x in range(1,n+1):
--     sumatoria = sumatoria + x
--   return sumatoria

-- def sumatoria(n):
--   return 0 if n == 0 else sumatoria(n-1) + n

f1 :: Integer -> Integer
f1 n
	| n == 0 = 1
	| n >= 1 = f1 (n-1) + 2^n 

f2 :: Integer -> Float -> Float
f2 n q
	  | n == 0 = 0
	  | n >= 1 = f2 (n-1) q + q^n 

f3 :: Integer -> Float -> Float
f3 n q  
		| n == 0 = 0
		| n >= 1 = f3(n-1) q + q^(2*n - 1) + q^(2*n)  

f4 :: Integer -> Float -> Float
f4 n q
	  | n == 0 = 0
	  | n >= 1 = f4(n-1) q + (q^(2*n - 1)) / 2 + (q^(2*n)) / 2

-- Implementar la función esPar :: Integer-> Bool que determine si un número natural es par. No está permitido utilizar mod ni div.

esPar :: Integer -> Bool
esPar n
	    | n == 0    = True
	    | otherwise = esImpar (abs n - 1)

esImpar :: Integer -> Bool
esImpar n 
		  | n == 0    = False 
		  | otherwise = esPar (abs n - 1) 

esPar' :: Integer -> Bool
esPar' n 
		| n == 0    = True
		| n == 1    = False
		| otherwise = not (esPar (abs n - 1)) 

-- Escribir una función para determinar si un número natural es múltiplo de 3. No está permitido utilizar mod ni div.

esMultiploDe3 :: Integer -> Bool
esMultiploDe3 n 
				| n == 0    = True
				| n <= 2    = False
				| otherwise = esMultiploDe3 (abs n - 3)

-- Implementar la función sumaImpares :: Integer-> Integer
-- que dado n ∈ N sume los primeros n números impares. Ej: sumaImpares 3 -> 1+3+5 = 9.

sumaImpares :: Integer -> Integer
sumaImpares n 
			  | n == 0 = 0
			  | otherwise = sumaImpares (n-1) + (2*n - 1)


-- def sumaImpares(n):
--   # Suma los primeros n números impares
--   sumaImpares = 0
--   iesimoImpar = 0
--   i = 1
--   while iesimoImpar < n:
--     if i % 2:
--       sumaImpares += i
--       iesimoImpar +=1
--     i += 1
--   return sumaImpares

-- Escribir una función doblefact que dado un número natural par calcula
-- n!! = n(n −2)(n −4)...2. Por ejemplo: doblefact 10 -> 10∗8∗6∗4∗2 = 3840.

doblefact :: Integer -> Integer
doblefact n 
			| n == 0    = 1
			| esPar n   = doblefact (n-1) * n
			| otherwise = doblefact (n-1)  


-- Escribir una función recursiva que no termine si se la ejecuta con números negativos
-- (y en cambio sí termine para el resto de los números).

noTerminaConNeg :: Integer -> Integer
noTerminaConNeg n 
				  | n == 0 = 0
				  | otherwise = noTerminaConNeg(n-1) + 1 

-- def dobleFactorial(par):
--   # Dado un número natural par n devuelve n!! = n(n-2)...2 
--   dobleFact = 1
--   for num in range(par,1,-2):
--     dobleFact *= num
--   return dobleFact