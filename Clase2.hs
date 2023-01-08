module Clase2
where

-- Implementar la función signo.

signo :: Int -> Int 
signo n | n > 0     = 1
		| n < 0     = -1
   	--  | otherwise = 0

signo' :: Int -> Int 
signo' 0  = undefined
signo' n = if n > 0 then 1 else -1

-- Implementar la función absoluto que calcula el valor absoluto de un número.

absoluto :: Int -> Int
absoluto n | n >= 0    = n 
		   | otherwise = -n

absoluto' :: Int -> Int
absoluto' n = if n >= 0 then n else -n 

-- Implementar la función maximo que devuelve el máximo entre dos números.

maximo :: Int -> Int -> Int 
maximo n m | n >= m    = n 
		   | otherwise = m

maximo' :: Int -> Int -> Int 
maximo' n m = if n >= m then n else m 

-- Implementar la función maximo3 que devuelve el máximo entre tres números.

maximo3 :: Int -> Int -> Int -> Int 
maximo3 n m t = maximo (maximo n m) t

maximo3' :: Int -> Int -> Int -> Int 
maximo3' n m t = if n >= m 
					then if n >= t then n else t  
					else if m >= t then m else t 

-- Tipos de datos. Ejercicios.

-- Tipar las siguientes funciones.

doble :: Int -> Int 
doble x = x + x

cuadruple :: Int -> Int 
cuadruple x = doble (doble x) 

-- esPar: dado un valor determina si es par o no.

esPar :: Int -> Bool
esPar n | mod n 2 == 0 = True 
		| otherwise    = False 

esPar' :: Int -> Bool
esPar' n = mod n 2 == 0

-- esMultiploDe: dados dos números naturales, determina si el primero es múltiplo del segundo.

esMultiploDe :: Int -> Int -> Bool
esMultiploDe n m = mod n m == 0 

-- normaVectorial: dado un vector determina su norma.

normaVectorial :: (Float,Float) -> Float 
normaVectorial (x1,x2) = sqrt(cuad x1 + cuad x2)

cuad :: Float -> Float
cuad x = x*x 

-- creaPar: crea un par a partir de sus dos componentes.

creaPar :: a -> b -> (a,b)
creaPar a b = (a,b) 

-- invertir: invierte el par pasado como parámetro.

invertir :: (a,b) -> (b,a)
invertir (a,b) = (b,a) 

-- distanciaPuntos: dados dos puntos del plano, calcula su distancia.

distanciaPuntos :: (Float,Float) -> (Float,Float) -> Float
distanciaPuntos p1 p2 = normaVectorial (fst p1 - fst p2,snd p1 - snd p2)  

-- Implementar las siguientes funciones:

f1 :: Float -> (Float,Float,Float)
f1 x = (2*x,x^2,x-7)

f2 :: Int -> Int 
f2 n | esPar n   = div n 2 
	 | otherwise = n + 1

f :: Int -> Int 
f n | esMultiploDe n 6 = div (n^2) 2 
	| otherwise        = 3*n + 1 

g :: (Int,Int) -> Int 
g (n,m) = n*(m + 1)

-- Composición h(f) = f(g(n,m))

h :: (Int,Int) -> Int 
h (n,m) = f(g (n,m)) 