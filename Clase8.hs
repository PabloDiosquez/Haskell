module Clase8
where

type Set a = [a]

vacio :: Set Integer
vacio = []

pertenece :: Integer -> Set Integer -> Bool
pertenece _ []     = False
pertenece x (c:cs) = x == c || pertenece x cs 

pertenece' :: Integer -> Set Integer -> Bool
pertenece' _ [] = False
pertenece' x (c:cs) = if x == c
						 then True
						 else pertenece' x cs


agregar :: Integer -> Set Integer -> Set Integer
agregar x c = if pertenece x c
				 then c
				 else x : c


-- Implementar la función incluido :: Set Int-> Set Int-> Bool que determina si el primer conjunto está
-- incluido en el segundo.

incluido :: Set Integer -> Set Integer -> Bool
incluido [] cs     = True
incluido (x:xs) cs = pertenece x cs && incluido xs cs 

-- Implementar la función iguales :: Set Int-> Set Int-> Bool que determina si dos conjuntos son iguales.

iguales :: Set Integer -> Set Integer -> Bool
iguales xs cs = (incluido xs cs) && (incluido cs xs) 

-- union:: Set Int-> Set Int-> Set Int que dado dos conjuntos, devuelve la unión entre ellos.

union :: Set Integer -> Set Integer -> Set Integer
union [] cs     = cs
union (x:xs) cs = union xs (agregar x cs) 

-- interseccion :: Set Int-> Set Int-> Set Int que dado dos conjuntos, devuelve la intersección entre ellos.

quitar :: Integer -> Set Integer -> Set Integer
quitar _ []     = []
quitar x (c:cs) = if x == c then cs else c : quitar x cs

interseccion :: Set Integer -> Set Integer -> Set Integer
interseccion [] _      = []
interseccion (x:xs) cs = if pertenece x cs
							then x : interseccion xs (quitar x cs)
							else interseccion xs cs

-- diferencia :: Set Int -> Set Int -> Set Int que dado los conjuntos A y B, devuelve A\B.

diferencia :: Set Integer -> Set Integer -> Set Integer
diferencia [] _ = [] 
diferencia (x:xs) cs 
					| not (pertenece x cs) = x : diferencia xs cs 
					| otherwise          = diferencia xs cs 

-- diferenciaSimetrica :: Set Int -> Set Int -> Set Int que dado los conjuntos A y B, devuelve la diferencia
-- simétrica.

diferenciaSimetrica :: Set Integer -> Set Integer -> Set Integer
diferenciaSimetrica xs cs = diferencia (union xs cs) (interseccion xs cs)