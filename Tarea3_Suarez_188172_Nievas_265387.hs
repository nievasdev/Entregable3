module Tarea3 where

---------------------------------
--Nombre 1	: Mauro Guillermo Nievas Peralta 
--Nro. 1	: 265387
---------------------------------
--Nombre 2 	: Roy Maximiliano Suarez Nilo
--Nro. 2 	: 188172
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
t = B (L 4) 8 (B (U 14 (L 9)) 16 (L 18))

--2)
sumTree :: Tree -> Int
sumTree (L x) = x
sumTree (U x t) = x + sumTree t
sumTree (B t1 x t2) = sumTree t1 + x + sumTree t2 

--3)
treeToList :: Tree -> [Int]
treeToList (L x) = [x]
treeToList (U x t) = treeToList t ++ [x]
treeToList (B t1 x t2) = treeToList t1 ++ [x] ++ treeToList t2

--4)
treeHeight :: Tree -> Int
treeHeight (L x) = 1
treeHeight (U x t) = 1 + treeHeight t
treeHeight (B t1 x t2) = 1 + max (treeHeight t1) (treeHeight t2)

--5)
completeLevel :: Tree -> Int -> Tree
completeLevel a 1 = a
completeLevel a 0 = a

completeLevel (L x) h = B (completeLevel (L x) (h-1)) x (completeLevel (L x) (h-1))

completeLevel (U x t) h = B (completeLevel t (h-1)) x (completeLevel t (h-1))

completeLevel (B t1 x t2) h = B (completeLevel t1 (h-1)) x (completeLevel t2 (h-1))

--6)
{--
Proposición: (∀t::Tree) treeHeight(completeLevel t (treeHeight t)) = treeHeight t

Demostración:
Sea t::Tree arbitrario. Sea h = treeHeight t.
Demostramos que treeHeight(completeLevel t h) = treeHeight t.

Procederemos por inducción estructural sobre t.

-----------------------------------------------------------
CASO BASE: t = L x

Sea h = treeHeight(L x) = 1.

  treeHeight(completeLevel (L x) h)
  = (Sustituyendo h = 1)
  treeHeight(completeLevel (L x) 1)
  = (Código de completeLevel, caso base: completeLevel a 1 = a)
  treeHeight(L x)
  ∎

-----------------------------------------------------------
PASO INDUCTIVO 1: t = U x t'

Hipótesis inductiva: treeHeight(completeLevel t' (treeHeight t')) = treeHeight t'.
Sea h = treeHeight(U x t') = 1 + treeHeight t'.

Demostramos por casos sobre h:

Subcaso h = 1:
  Entonces treeHeight(U x t') = 1, lo cual implica que t' debe tener altura 0.
  Pero no existe árbol de altura 0 en nuestro tipo Tree.
  Por lo tanto, este caso es vacío.

Subcaso h >= 2:
  treeHeight(completeLevel (U x t') h)
  = (Código de completeLevel, caso U optimizado: usa t' en ambos lados)
  treeHeight(B (completeLevel t' (h-1)) x (completeLevel t' (h-1)))
  = (Código de treeHeight, caso B)
  1 + max (treeHeight(completeLevel t' (h-1))) (treeHeight(completeLevel t' (h-1)))

  Observemos que:
  - h = 1 + treeHeight t', entonces h - 1 = treeHeight t'
  - Por lo tanto, h - 1 >= treeHeight t' (de hecho es igual)

  Aplicamos el Lema Auxiliar (ver final) a ambos subárboles:
  Como h-1 >= treeHeight t', entonces treeHeight(completeLevel t' (h-1)) = h-1.

  Sustituyendo:
  = 1 + max (h-1) (h-1)
  = (Lema L1: max x x = x)
  1 + (h-1)
  = h
  = treeHeight(U x t')
  ∎

-----------------------------------------------------------
PASO INDUCTIVO 2: t = B t1 x t2

Hipótesis inductiva:
  treeHeight(completeLevel t1 (treeHeight t1)) = treeHeight t1
  treeHeight(completeLevel t2 (treeHeight t2)) = treeHeight t2

Sea h = treeHeight(B t1 x t2) = 1 + max (treeHeight t1) (treeHeight t2).

Subcaso h = 1:
  Este caso es vacío (similar al caso U).

Subcaso h >= 2:
  treeHeight(completeLevel (B t1 x t2) h)
  = (Código de completeLevel, caso B con h >= 2)
  treeHeight(B (completeLevel t1 (h-1)) x (completeLevel t2 (h-1)))
  = (Código de treeHeight, caso B)
  1 + max (treeHeight(completeLevel t1 (h-1))) (treeHeight(completeLevel t2 (h-1)))

  Observemos que h - 1 = max (treeHeight t1) (treeHeight t2).

  Como h-1 >= max (treeHeight t1) (treeHeight t2), y aplicando la lógica inductiva
  sobre los subárboles completados hasta h-1:

  - treeHeight(completeLevel t1 (h-1)) = max (treeHeight t1) (h-1) = h-1
  - treeHeight(completeLevel t2 (h-1)) = max (treeHeight t2) (h-1) = h-1

  Entonces:
  = 1 + max (h-1) (h-1)
  = (Lema L1: max x x = x)
  1 + (h-1)
  = h
  = treeHeight(B t1 x t2)
  ∎

-----------------------------------------------------------
Lema auxiliar (usado arriba):
Para todo árbol a::Tree y todo n::Int con n >= 1:
Si n >= treeHeight a, entonces treeHeight(completeLevel a n) = n.

(Este lema se puede demostrar por inducción sobre n, pero se omite por brevedad)

-----------------------------------------------------------
Por el principio de inducción estructural sobre Tree, concluimos:
(∀t::Tree) treeHeight(completeLevel t (treeHeight t)) = treeHeight t ∎

--}
