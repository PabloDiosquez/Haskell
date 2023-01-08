module Clase7
where

-- Definir la función listar :: a-> a-> a-> [a] que toma 3 elementos y los convierte en una lista.

listar :: a -> a -> a -> [a]
listar a b c = [a,b,c]

-- Escribir una expresión que denote la lista estrictamente decreciente de enteros que comienza con
-- el número 1 y termina con el número -100.

-- [1,0..(-100)]

-- sumatoria :: [Integer]-> Integer que indica la suma de los elementos de una lista.

sumatoria :: [Integer] -> Integer
sumatoria xs 
			  | xs == [] = 0
			  | otherwise = sumatoria (tail xs) + head xs

sumatoria' :: [Integer] -> Integer
sumatoria' xs = if xs == []
				 then 0 
				 else sumatoria (tail xs) + head xs

-- pertenece :: Integer-> [Integer] -> Bool que indica si un elemento aparece en la lista.
-- Por ejemplo: 
-- pertenece 9 [] False
-- pertenece 9 [1,2,3] False 
-- pertenece 9 [1,2,9,9,-1,0] True

pertenece :: Integer -> [Integer] -> Bool
pertenece n xs 
			   | xs == []     = False
			   | n == head xs = True
			   | otherwise    = pertenece n (tail xs)

-- Definir la función longitud :: [Integer] -> Integer, que dada una lista de enteros, devuelve su longitud.

longitud :: [Integer] -> Integer
longitud xs = if xs == []
				 then 0 
				 else longitud (tail xs) + 1

-- ¿Cómo escribir la función sumatoria :: [Integer] -> Integer usando pattern matching?

sumatoriaPM :: [Integer] -> Integer
sumatoriaPM []     = 0
sumatoriaPM (x:xs) = sumatoriaPM xs + x

-- Repensar la función pertenece utilizando pattern matching.

pertenecePM :: Integer -> [Integer] -> Bool
pertenecePM _ []     = False
pertenecePM n (x:xs) = n == x || pertenecePM n xs 

-- ¿Cómo escribir la función longitud :: [Integer] -> Integer usando pattern matching?

longitudPM :: [Integer] -> Integer
longitudPM []     = 0
longitudPM (x:xs) = longitudPM xs + 1

-- productoria :: [Integer] -> Integer que devuelve la productoria de los elementos.

productoria :: [Integer] -> Integer
productoria []     = 1
productoria (x:xs) = x*(productoria xs)

-- sumarN :: Integer -> [Integer]-> [Integer] que dado un número N y una lista xs, suma N a cada elemento de xs.

sumarN :: Integer -> [Integer] -> [Integer]
sumarN _ []     = []
sumarN n (x:xs) = (n+x) : sumarN n xs

-- sumarElUltimo :: [Integer]-> [Integer] que dada una lista no vacía xs, suma el último elemento a cada elemento
-- de xs. Ejemplo sumarElUltimo [1,2,3] [4,5,6]

sumarElUltimo :: [Integer] -> [Integer] 
sumarElUltimo xs = sumarN (last xs) xs

-- sumarElPrimero :: [Integer] -> [Integer] que dada una lista no vacía xs, suma el primer elemento a cada elemento
-- de xs. Ejemplo sumarElPrimero [1,2,3] [2,3,4]

sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero xs = sumarN (head xs) xs

-- pares :: [Integer]-> [Integer] que devuelve una lista con los elementos pares de la lista original. 
-- Ejemplo pares [1,2,3,8] [2,8]

pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs) = if esPar x
				 then x : pares xs
				 else pares xs
 				where esPar x = mod x 2 == 0

-- multiplosDeN :: Integer -> [Integer] -> [Integer] que dado un número N y una lista xs, devuelve una lista con los
-- elementos múltiplos N de xs.

multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN n (x:xs) = if esMultiploDe x n
						 then x : multiplosDeN n xs
						 else multiplosDeN n xs
						 where esMultiploDe x n = mod x n == 0

-- quitar :: Integer-> [Integer]-> [Integer] que elimina la primera aparición del elemento
-- en la lista (de haberla).

quitar :: Integer -> [Integer] -> [Integer]
quitar e (x:xs) = if e == x 
					then xs
					else x : quitar e xs

-- hayRepetidos :: [Integer] -> Bool que indica si una lista tiene elementos repetidos.

hayRepetidos :: [Integer] -> Bool
hayRepetidos []                      = False
hayRepetidos (x:xs) | pertenece x xs = True
					| otherwise      = hayRepetidos xs

-- eliminarRepetidos :: [Integer] -> [Integer] que deja en la lista una única aparición de cada elemento, eliminando
-- las repeticiones adicionales.

quitarTodas :: Integer -> [Integer] -> [Integer]
quitarTodas _ []     = []
quitarTodas n (x:xs) = if n == x 
						 then quitarTodas n xs 
						 else x : quitarTodas n xs

eliminarRepetidos :: [Integer] -> [Integer]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | pertenece x xs = x : eliminarRepetidos (quitarTodas x xs)
						 | otherwise      = x : eliminarRepetidos xs

--maximo :: [Integer] -> Integer que calcula el máximo elemento de una lista no vacía.

maximo :: [Integer] -> Integer
maximo (x:xs) = maximoAux x xs 

maximoAux :: Integer -> [Integer] -> Integer
maximoAux n []     = n 
maximoAux n (x:xs) = if n > x 
						then maximoAux n xs 
						else maximoAux x xs 
 
-- ordenar :: [Integer]-> [Integer] que ordena los elementos de forma creciente.

minimoAux :: Integer -> [Integer] -> Integer
minimoAux n []     = n 
minimoAux n (x:xs) = if n < x 
						then minimoAux n xs
						else minimoAux x xs

minimo :: [Integer] -> Integer
minimo (x:xs) = minimoAux x xs 

ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar xs = minimo xs : ordenar (quitar (minimo xs) xs) 