module TP1
where

-- 1.
-- Conceptos básicos

-- Dado un número duvuelve su sucesor.

sucesor :: Int -> Int
sucesor x = x + 1

-- sumar :: Int -> Int -> Int
-- Dados dos números devuelve su suma utilizando la operación +.

sumar :: Int -> Int -> Int 
sumar num1 num2 = num1 + num2 

-- maximo :: Int -> Int -> Int
-- Dados dos números devuelve el mayor de estos.

maximo :: Int -> Int -> Int 
maximo x y = if x >= y
			   then x
			   else y 

-- 2.
-- Pattern matching

-- negar :: Bool -> Bool
-- Dado un booleano, si es True devuelve False, y si es False devuelve True.
-- Defininida en Haskell como not.

negar :: Bool -> Bool
negar True  = False 
negar False = True

-- b) andLogico :: Bool -> Bool -> Bool
-- Dados dos booleanos si ambos son True devuelve True, sino devuelve False.
-- Definida en Haskell como &&.

andLogico :: Bool -> Bool -> Bool
andLogico True True = True
andLogico _ _       = False

-- c) orLogico :: Bool -> Bool -> Bool
-- Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False.
-- Definida en Haskell como ||.

orLogico :: Bool -> Bool -> Bool
orLogico False False = False
orLogico _ _         = True

-- d) primera :: (Int,Int) -> Int
-- Dado un par de números devuelve la primera componente.
-- Definida en Haskell como fst.

primera :: (Int,Int) -> Int
primera (num1,num2) = num1

-- e) segunda :: (Int,Int) -> Int
-- Dado un par de números devuelve la segunda componente.
-- Definida en Haskell como snd.

segunda :: (Int,Int) -> Int
segunda (num1,num2) = num2

-- f ) sumaPar :: (Int,Int) -> Int
-- Dado un par de números devuelve su suma.

sumaPar :: (Int,Int) -> Int
sumaPar (num1,num2) = num1 + num2

-- g) maxDelPar :: (Int,Int) -> Int
-- Dado un par de números devuelve el mayor de estos.

maxDelPar :: (Int,Int) -> Int
maxDelPar (num1,num2)
					  | num1 >= num2 = num1
					  | otherwise    = num2 

-- maxDelPar (num1,num2) = maximo num1 num2

-- 3.
-- Funciones polimórficas.

-- loMismo :: a -> a
-- Dado un elemento de algún tipo devuelve ese mismo elemento.

loMismo :: a -> a
loMismo x = x

-- b) siempreSiete :: a -> Int
-- Dado un elemento de algún tipo devuelve el número 7.

siempreSiete :: a -> Int
siempreSiete x = 7

-- c) duplicar :: a -> (a,a)
-- Dado un elemento de algún tipo devuelve un par con ese elemento en ambas compo-
-- nentes.

duplicar :: a -> (a,a)
duplicar x = (x,x)

-- d) singleton :: a -> [a]
-- Dado un elemento de algún tipo devuelve una lista con este único elemento.

singleton :: a -> [a]
singleton x = [x] 


-- 4.
-- Funciones polimórficas. Pattern matching.

-- Defina las siguientes funciones polimórficas utilizando pattern matching sobre listas:
-- a) isEmpty :: [a] -> Bool
-- Dada una lista de elementos, si es vacía devuelve True, sino devuelve False.

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _  = False

-- b) head' :: [a] -> a
-- Dada una lista devuelve su primer elemento.
-- Precondición:
			-- La lista debe ser no vacía.

head' :: [a] -> a
head' (x:xs) = x 

-- c) tail' :: [a] -> [a]
-- Dada una lista devuelve esa lista menos el primer elemento.
-- Precondición:
			-- La lista debe ser no vacía

tail' :: [a] -> [a]
tail' (x:xs) = xs 

-- 5.
-- Recursión.

-- 1. sumatoria :: [Int] -> Int
-- Dada una lista de enteros devuelve la suma de todos sus elementos.

sumatoria :: [Int] -> Int
sumatoria []     = 0
sumatoria (x:xs) = x + sumatoria xs  

-- 2. longitud :: [a] -> Int
-- Dada una lista de elementos de algúin tipo devuelve el largo de esa lista, es decir, la cantidad
-- de elementos que posee.

longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs 

-- 3. promedio :: [Int] -> Int
-- Dada una lista de enteros, devuelve un número que es el promedio entre todos los elementos
-- de la lista. ¿Pudo resolverla utilizando recursión estructural?

promedio :: [Int] -> Int
promedio xs = div (sumatoria xs) (longitud xs)

-- 4. mapSucesor :: [Int] -> [Int]
-- Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.

mapSucesor :: [Int] -> [Int]
mapSucesor []     = []
mapSucesor (x:xs) = sucesor x : mapSucesor xs

-- 5. mapSumaPar :: [(Int,Int)] -> [Int]
-- Dada una lista de pares de enteros, devuelve una nueva lista en la que cada elemento es la
-- suma de los elementos de cada par.


mapSumaPar :: [(Int,Int)] -> [Int]
mapSumaPar []     = []
mapSumaPar (x:xs) = sumar (primera x) (segunda x) : mapSumaPar xs

-- 6. mapMaxDelPar :: [(Int,Int)] -> [Int]
-- Dada una lista de pares, devuelve una nueva lista en la que cada elemento es el mayor de
-- las componentes de cada par.

mapMaxDelPar :: [(Int,Int)] -> [Int]
mapMaxDelPar []     = []
mapMaxDelPar (x:xs) = maxDelPar x : mapMaxDelPar xs 

-- 7. todoVerdad :: [Bool] -> Bool
-- Dada una lista de booleanos devuelve True si todos sus elementos son True.

-- todoVerdad [True,True,True] -> True && todoVerdad [True,True] -> True $$ True && todoVerdad [True]
-- -> True && True && True && todoVerdad []

-- todoVerdad [True] = True 

todoVerdad :: [Bool] -> Bool
todoVerdad []     = True 
todoVerdad (x:xs) = x && todoVerdad xs 

-- 8. algunaVerdad :: [Bool] -> Bool
-- Dada una lista de booleanos devuelve True si alguno de sus elementos es True.

algunaVerdad :: [Bool] -> Bool
algunaVerdad []     = False
algunaVerdad (x:xs) = x || algunaVerdad xs

-- 9. pertenece :: Eq a => a -> [a] -> Bool
-- Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual
-- a e.

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ []     = False 
pertenece e (x:xs) = e == x || pertenece e xs  

-- 10. apariciones :: Eq a => a -> [a] -> Int
-- Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.

apariciones :: Eq a => a -> [a] -> Int
apariciones _ []     = 0 
apariciones e (x:xs) = if e == x 
						then 1 + apariciones e xs 
						else apariciones e xs

-- 11. filtrarMenoresA :: Int -> [Int] -> [Int]
-- Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.

filtrarMenoresA :: Int -> [Int] -> [Int]
filtrarMenoresA _ [] = [] 
filtrarMenoresA n (x:xs) 
						| x < n     = x : filtrarMenoresA n xs
						| otherwise = filtrarMenoresA n xs

-- 12. filtrarElemento :: Eq a => a -> [a] -> [a]
-- Dados un elemento y una lista (elimina) todas las ocurrencias de ese elemento en la
-- lista.

filtrarElemento :: Eq a => a -> [a] -> [a]
filtrarElemento _ []     = []
filtrarElemento e (x:xs) = if e == x
							  then filtrarElemento e xs
							  else x : filtrarElemento e xs

-- 13. mapLongitudes :: [[a]] -> [Int]
-- Dada una lista de listas, devuelve la lista de sus longitudes. Aplique esta función a la lista
-- de strings ["Estructuras", "de", "datos"] y observe el resultado.

mapLongitudes :: [[a]] -> [Int]
mapLongitudes []       = [] 
mapLongitudes (xs:xss) = longitud xs : mapLongitudes xss

-- 14. longitudMayorA :: Int -> [[a]] -> [[a]]
-- Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más
-- de n elementos.

longitudMayorA :: Int -> [[a]] -> [[a]]
longitudMayorA _ []       = []
longitudMayorA n (xs:xss) = if longitud xs > n
							  then xs : longitudMayorA n xss 
							  else longitudMayorA n xss 

-- 15. intercalar :: a -> [a] -> [a]
-- Dado un elemento e y una lista xs, ubica a e entre medio de todos los elementos de xs.
-- Ejemplo:
-- intercalar ',' "abcde" == "a,b,c,d,e"



-- 16. snoc :: [a] -> a -> [a]
-- Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la
-- lista.

snoc :: [a] -> a -> [a]
snoc xs a = if longitud xs == 0 
			   then a : xs
			   else head xs : snoc (tail xs) a
 
-- 17. append :: [a] -> [a] -> [a]
-- Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
-- elementos de la segunda a continuación. Definida en Haskell como ++.

append :: [a] -> [a] -> [a]
append xs [] = xs
append xs (e:es) = append (snoc xs e) es 

-- 18. aplanar :: [[a]] -> [a]
-- Dada una lista de listas, devuelve una única lista con todos sus elementos.

aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (xs:xss) = append xs (aplanar xss)

-- 19. reversa :: [a] -> [a]
-- Dada una lista devuelve la lista con los mismos elementos de atr+as para adelante. Definida
-- en Haskell como reverse.

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = snoc (reversa xs) x

-- 20. zipMaximos :: [Int] -> [Int] -> [Int]
-- Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
-- máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
-- las listas no necesariamente tienen la misma longitud.


zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos _ [] = []
zipMaximos [] _ = []
zipMaximos (x:xs) (e:es) = maximo x e : zipMaximos xs es 


-- 21. zipSort :: [Int] -> [Int] -> [(Int, Int)]
-- Dadas dos listas de enteros de igual longitud, devuelve una lista de pares (min; max), donde
-- min y max son el mínimo y el máximo entre los elementos de ambas listas en la misma
-- posición.

-- Precondición:
		-- Las listas deben tener la misma longitud.

minimo :: Int -> Int -> Int 
minimo x y = if x <= y then x else y


zipSort :: [Int] -> [Int] -> [(Int,Int)]
zipSort [] _ = []
zipSort (x:xs) (e:es) = (maximo x e,minimo x e) : zipSort xs es 

-- Defina las siguientes funciones utilizando recursión sobre números enteros, salvo que se indique
-- lo contrario:

-- 1. factorial :: Int -> Int
-- Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta
-- llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.

factorial :: Int -> Int 
factorial 0 = 1
factorial n = factorial(n-1)*n

factorial' :: Int -> Int
factorial' n = if n == 0
				 then 1
				 else factorial'(n-1)*n

-- 2. cuentaRegresiva :: Int -> [Int]
-- Dado un número n devuelve una lista cuyos elementos son los números comprendidos entre
-- n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva n | n < 1 = []
cuentaRegresiva n         = n : cuentaRegresiva(n-1) 

-- 3. contarHasta :: Int -> [Int]
-- Dado un número n devuelve una lista cuyos elementos sean los números entre 1 y n (inclui-
-- dos).

contarHasta :: Int -> [Int]
contarHasta 0 = []
contarHasta n = snoc (contarHasta(n-1)) n 

-- 4. replicarN :: Int -> a -> [a]
-- Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.

replicarN :: Int -> a -> [a]
replicarN 0 _ = []
replicarN n e = e : replicarN (n-1) e

-- 5. desdeHasta :: Int -> Int -> [Int]
-- Dados dos números n y m devuelve una lista cuyos elementos sean los números entre n y m
-- (incluidos).

desdeHasta :: Int -> Int -> [Int]
desdeHasta n m | n > m = [] 
desdeHasta n m         = snoc (desdeHasta n (m-1)) m 


-- 6. takeN :: Int -> [a] -> [a]
-- Dados un número n y una lista xs, devuelve una lista con los primeros n elementos de xs.
-- Si xs posee menos de n elementos, se devuelve la lista completa.

takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n xs = if n > longitud xs
				then xs 
				else (head xs) : (takeN (n-1) (tail xs))  

-- 7. dropN :: Int -> [a] -> [a]
-- Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista
-- recibida. Si la lista posee menos de n elementos, se devuelve una lista vacía.

dropN :: Int -> [a] -> [a]
dropN n xs | longitud xs < n = []
dropN n (x:xs)               = if n == 0 
								then (x:xs)
								else dropN (n-1) xs

-- 8. splitN :: Int -> [a] -> ([a], [a])
-- Dados un número n y una lista xs, devuelve un par donde la primera componente es la lista
-- que resulta de aplicar takeN a xs, y la segunda componente el resultado de aplicar dropN
-- a xs. ¿Conviene utilizar recursión?

splitN :: Int -> [a] -> ([a],[a])
splitN n xs = (takeN n xs, dropN n xs)

-- Defina las siguientes funciones:
-- 1. particionPorSigno :: [Int] -> ([Int], [Int])
-- Dada una lista xs de enteros devuelva una tupla de listas, donde la primera componente con-
-- tiene todos aquellos números positivos de xs y la segunda todos aquellos números negativos
-- de xs. ¿Conviene utilizar recursión? Considere utilizar funciones auxiliares.

positivos :: [Int] -> [Int]
positivos [] = []
positivos (x:xs) = if x > 0
					 then x : positivos xs 
					 else positivos xs 

negativos :: [Int] -> [Int]
negativos [] = []
negativos (x:xs) = if x < 0 
					then x : negativos xs 
					else negativos xs 

particionPorSigno :: [Int] -> ([Int],[Int])
particionPorSigno xs = (positivos xs, negativos xs) 


-- 2. particionPorParidad :: [Int] -> ([Int], [Int])
-- Dada una lista xs de enteros devuelva una tupla de listas, donde la primera componente
-- contiene todos aquellos números pares de xs y la segunda todos aquellos números impares
-- de xs. ¿Conviene utilizar recursión? Considere utilizar funciones auxiliares.

pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) = if esPar x 
				then x : pares xs 
				else pares xs
				where esPar x = mod x 2 == 0

impares :: [Int] -> [Int]
impares [] = []
impares (x:xs) = if esImpar x 
					then x : impares xs 
					else impares xs
					where esImpar x = mod x 2 == 1 


particionPorParidad :: [Int] -> ([Int],[Int])
particionPorParidad xs = (pares xs, impares xs)


-- 3. subtails :: [a] -> [[a]]
-- Dada una lista devuelve cada sublista resultante de aplicar tail en cada paso. Ejemplo:
-- subtails "abc" == ["abc", "bc", "c",""]

subtails :: [a] -> [[a]]
subtails [] = [[]]
subtails xs = xs : subtails (tail xs)  

-- 4. agrupar :: Eq a => [a] -> [[a]]
-- Dada una lista xs devuelve una lista de listas donde cada sublista contiene elementos conti-
-- guos iguales de xs. Ejemplo:
-- agrupar "AABCCC" = ["AA","B","CC"]

repiteN :: a -> Int -> [a]
repiteN _ 0 = [] 
repiteN e n = e : repiteN e (n-1)

contarRepeticiones :: Eq a => a -> [a] -> Int
contarRepeticiones _ [] = 0 
contarRepeticiones e (x:xs) = if e == x
								then 1 + contarRepeticiones e xs
								else contarRepeticiones e xs 

quitarTodas :: Eq a => a -> [a] -> [a]
quitarTodas _ []     = []
quitarTodas e (x:xs) = if e == x then quitarTodas e xs else x : quitarTodas e xs


agrupar ::  Eq a => [a] -> [[a]]
agrupar []     = []
agrupar (x:xs) = repiteN x (contarRepeticiones x (x:xs)) : agrupar (quitarTodas x xs) 


-- 5. esPrefijo :: Eq a => [a] -> [a] -> Bool
-- Devuelve True si la primera lista es prefijo de la segunda.

esPrefijo :: Eq a => [a] -> [a] -> Bool 
esPrefijo [] _          = True
esPrefijo (x:xs) (e:es) = x == e && esPrefijo xs es

-- 6. esSufijo :: Eq a => [a] -> [a] -> Bool
-- Devuelve True si la primera lista es sufijo de la segunda.