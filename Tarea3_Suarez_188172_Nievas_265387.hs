module Tarea3 where

---------------------------------
--Nombre 1	: 
--Nro. 1	: 
---------------------------------
--Nombre 2 	: 
--Nro. 2 	: 
---------------------------------

--------------------
----Ejercicio 1:----
--------------------


--1)
unir:: [a] -> [a] -> [a]
unir l1 l2 = l1 ++ l2

--2)
producto :: [Int] -> Int
producto [] = 1
producto (x:xs) = x * producto xs

--3)
{--
Ejercicio 1.3 - Demostración por Inducción

Proposición: (∀ l1,l2::[Int]) producto(unir l1 l2) = (producto l1)*(producto l2)

La propiedad a demostrar es:
P l1 ≡ (∀l2::[Int]) producto(unir l1 l2) = (producto l1)*(producto l2)

Procederemos por inducción sobre l1.

-----------------------------------------------------------
CASO BASE: Tesis: (∀l2::[Int]) producto(unir [] l2) = (producto [])*(producto l2)

Demostración:
Consideramos l2::[Int] arbitraria y demostramos producto(unir [] l2) = (producto [])*(producto l2).

Lado izquierdo:
producto(unir [] l2)
= (Código de unir, con l1 = [])
producto([] ++ l2)
= (Código de ++, caso base)
producto l2

Lado derecho:
(producto []) * (producto l2)
= (Código de producto, caso base)
1 * (producto l2)
= (Aritmética)
producto l2

Ambos lados dan producto l2. ∎

-----------------------------------------------------------
PASO INDUCTIVO:
Hipótesis: Sea l1::[Int] tal que (∀l2::[Int]) producto(unir l1 l2) = (producto l1)*(producto l2)
Sea x::Int
Tesis: (∀l2::[Int]) producto(unir (x:l1) l2) = (producto (x:l1))*(producto l2)

Demostración:
Consideramos l2::[Int] arbitraria y demostramos producto(unir (x:l1) l2) = (producto (x:l1))*(producto l2).

Lado izquierdo:
producto(unir (x:l1) l2)
= (Código de unir)
producto((x:l1) ++ l2)
= (Código de ++, caso recursivo)
producto(x : (l1 ++ l2))
= (Código de producto, caso recursivo)
x * producto(l1 ++ l2)
= (Definición de unir: unir l1 l2 = l1 ++ l2)
x * producto(unir l1 l2)
= (Hipótesis de inducción)
x * ((producto l1) * (producto l2))
= (Asociatividad de *)
(x * producto l1) * (producto l2)

Lado derecho:
(producto (x:l1)) * (producto l2)
= (Código de producto, caso recursivo)
(x * producto l1) * (producto l2)

Ambos lados llegan a la misma expresión. ∎

-----------------------------------------------------------
Por el principio de inducción sobre listas, concluimos:
(∀ l1,l2::[Int]) producto(unir l1 l2) = (producto l1)*(producto l2) ∎
-}

--------------------
----Ejercicio 2:----
--------------------
--1)
cumplen :: (a -> Bool) -> [a] -> [a]
cumplen p [] = []
cumplen p (x:xs) = if p x then x : cumplen p  xs else cumplen p xs  

--2)
descartar :: (a -> Bool) -> [a] -> [a]
descartar p [] = []
descartar p (x:xs) = if p x then descartar p xs else x : descartar p xs

--3)
{--
Ejercicio 2.3 - Demostración por Inducción

Proposición: (∀xs::[a]) (∀p::(a -> Bool)) length(cumplen p xs) + length(descartar p xs) = length xs

La propiedad a demostrar es:
P xs ≡ (∀p::(a -> Bool)) length(cumplen p xs) + length(descartar p xs) = length xs

Procederemos por inducción sobre xs.

-----------------------------------------------------------
CASO BASE: Tesis: (∀p::(a -> Bool)) length(cumplen p []) + length(descartar p []) = length []

Demostración:
Consideramos p::(a -> Bool) arbitrario y demostramos length(cumplen p []) + length(descartar p []) = length [].

Lado izquierdo:
length(cumplen p []) + length(descartar p [])
= (Código de cumplen, caso base)
length [] + length(descartar p [])
= (Código de descartar, caso base)
length [] + length []
= (Código de length, caso base)
0 + 0
= (Aritmética)
0

Lado derecho:
length []
= (Código de length, caso base)
0

Ambos lados dan 0. ∎

-----------------------------------------------------------
PASO INDUCTIVO:
Hipótesis: Sea xs::[a] tal que (∀p::(a -> Bool)) length(cumplen p xs) + length(descartar p xs) = length xs
Sea x::a
Tesis: (∀p::(a -> Bool)) length(cumplen p (x:xs)) + length(descartar p (x:xs)) = length (x:xs)

Demostración:
Consideramos p::(a -> Bool) arbitrario y demostramos length(cumplen p (x:xs)) + length(descartar p (x:xs)) = length (x:xs).

Primero calculamos el lado derecho:
length (x:xs)
= (Código de length, caso recursivo)
1 + length xs

Ahora calcularemos el lado izquierdo. Como las funciones cumplen y descartar están definidas por casos según valga o no p x,
dividimos la demostración en dos casos:

--- Caso 1: p x es verdadero ---

Lado izquierdo:
length(cumplen p (x:xs)) + length(descartar p (x:xs))
= (Código de cumplen, caso recursivo con p x verdadero)
length(x : cumplen p xs) + length(descartar p (x:xs))
= (Código de descartar, caso recursivo con p x verdadero)
length(x : cumplen p xs) + length(descartar p xs)
= (Código de length, caso recursivo)
(1 + length(cumplen p xs)) + length(descartar p xs)
= (Aritmética)
1 + (length(cumplen p xs) + length(descartar p xs))
= (Hipótesis de inducción)
1 + length xs

Que es donde deseábamos arribar. ∎

--- Caso 2: p x es falso ---

Lado izquierdo:
length(cumplen p (x:xs)) + length(descartar p (x:xs))
= (Código de cumplen, caso recursivo con p x falso)
length(cumplen p xs) + length(descartar p (x:xs))
= (Código de descartar, caso recursivo con p x falso)
length(cumplen p xs) + length(x : descartar p xs)
= (Código de length, caso recursivo)
length(cumplen p xs) + (1 + length(descartar p xs))
= (Aritmética)
1 + (length(cumplen p xs) + length(descartar p xs))
= (Hipótesis de inducción)
1 + length xs

Que es donde deseábamos arribar. ∎

-----------------------------------------------------------
Por el principio de inducción sobre listas, concluimos:
(∀xs::[a]) (∀p::(a -> Bool)) length(cumplen p xs) + length(descartar p xs) = length xs ∎
-}

--------------------
----Ejercicio 3:----
--------------------

data Tree = L Int | U Int Tree | B Tree Int Tree
 deriving Show
 
--1)
t :: Tree
t = undefined

--2)
sumTree :: Tree -> Int
sumTree t = undefined 

--3)
treeToList :: Tree -> [Int]
treeToList t = undefined

--4)
treeHeight :: Tree -> Int
treeHeight t = undefined

--5) 
completeLevel :: Tree -> Int -> Tree
completeLevel t h = undefined

--6)
{--
(∀t::Tree)(∀h::Int) treeHeight(completeLevels t h) = treeHeight t

--}
