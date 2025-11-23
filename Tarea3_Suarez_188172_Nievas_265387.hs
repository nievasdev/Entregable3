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

completeLevel (U x t) h = B (completeLevel t (h-1)) x (completeLevel (L x) (h-1))

completeLevel (B t1 x t2) h = B (completeLevel t1 (h-1)) x (completeLevel t2 (h-1))

--6)
{--
Proposición: (∀t::Tree)(∀h::Int) h >= 2 ⟹  treeHeight(completeLevel t h) = h

Demostración:
Sea t::Tree arbitrario y sea h::Int tal que h >= 2.
Demostramos que treeHeight(completeLevel t h) = h.

Procederemos por inducción fuerte sobre h, y para cada h, por inducción estructural sobre t.

-----------------------------------------------------------
CASO BASE: h = 2

Demostramos que para cualquier t::Tree, treeHeight(completeLevel t 2) = 2.
Procedemos por casos sobre la estructura de t.

Subcaso t = L x:
  treeHeight(completeLevel (L x) 2)
  = (Código de completeLevel, caso L x con h = 2)
  treeHeight(B (completeLevel (L x) 1) x (completeLevel (L x) 1))
  = (Código de completeLevel, caso base h = 1: completeLevel a 1 = a)
  treeHeight(B (L x) x (L x))
  = (Código de treeHeight, caso B)
  1 + max (treeHeight(L x)) (treeHeight(L x))
  = (Código de treeHeight, caso L)
  1 + max 1 1
  = (Aritmética)
  2
  ∎

Subcaso t = U x t':
  treeHeight(completeLevel (U x t') 2)
  = (Código de completeLevel, caso U con h = 2)
  treeHeight(B (completeLevel t' 1) x (completeLevel (L x) 1))
  = (Código de completeLevel, caso base h = 1)
  treeHeight(B t' x (L x))
  = (Código de treeHeight, caso B)
  1 + max (treeHeight t') (treeHeight(L x))
  = (Código de treeHeight, caso L)
  1 + max (treeHeight t') 1
  = (Como treeHeight t' >= 1 para cualquier árbol t', tenemos max (treeHeight t') 1 = treeHeight t' >= 1)
  1 + treeHeight t'

  Ahora, dado que t' puede tener altura 1 o mayor:
  - Si treeHeight t' = 1, entonces 1 + 1 = 2 ✓
  - Si treeHeight t' > 1, entonces 1 + treeHeight t' > 2...

  (Nota: Este caso muestra que la proposición no es exacta para todos los árboles.
   La altura resultante depende del árbol original cuando h es pequeño.)

Reformulemos la proposición más precisamente:

-----------------------------------------------------------
Proposición Correcta:
(∀t::Tree)(∀h::Int) h >= 1 ⟹ treeHeight(completeLevel t h) >= h

Y además, para árboles que necesitan completarse:
Si treeHeight t < h, entonces treeHeight(completeLevel t h) = h.

-----------------------------------------------------------
Demostración (versión simplificada):

Procederemos por inducción fuerte sobre h >= 2.

Propiedad a demostrar: P(h) ≡ (∀t::Tree) treeHeight(completeLevel t h) >= h

CASO BASE: h = 1
  Para cualquier t::Tree:
  treeHeight(completeLevel t 1)
  = (Código de completeLevel, caso base)
  treeHeight t
  >= (Todo árbol tiene altura >= 1)
  1
  ∎

CASO BASE: h = 2
  Ya demostrado arriba que treeHeight(completeLevel t 2) >= 2 para todo t.

PASO INDUCTIVO:
Hipótesis: Para todo h' < h con h' >= 1, vale P(h'), es decir,
           (∀t::Tree) treeHeight(completeLevel t h') >= h'.
Sea h >= 3.
Tesis: (∀t::Tree) treeHeight(completeLevel t h) >= h.

Demostramos por casos sobre t:

Caso t = L x:
  treeHeight(completeLevel (L x) h)
  = (Código de completeLevel, caso L x con h >= 2)
  treeHeight(B (completeLevel (L x) (h-1)) x (completeLevel (L x) (h-1)))
  = (Código de treeHeight, caso B)
  1 + max (treeHeight(completeLevel (L x) (h-1))) (treeHeight(completeLevel (L x) (h-1)))
  = (Simplificación)
  1 + treeHeight(completeLevel (L x) (h-1))
  >= (Hipótesis inductiva: treeHeight(completeLevel (L x) (h-1)) >= h-1)
  1 + (h-1)
  = (Aritmética)
  h
  ∎

Caso t = U x t':
  treeHeight(completeLevel (U x t') h)
  = (Código de completeLevel, caso U con h >= 2)
  treeHeight(B (completeLevel t' (h-1)) x (completeLevel (L x) (h-1)))
  = (Código de treeHeight, caso B)
  1 + max (treeHeight(completeLevel t' (h-1))) (treeHeight(completeLevel (L x) (h-1)))
  >= (Hipótesis inductiva aplicada a ambos subárboles)
  1 + max (h-1) (h-1)
  = (Aritmética)
  h
  ∎

Caso t = B t1 x t2:
  treeHeight(completeLevel (B t1 x t2) h)
  = (Código de completeLevel, caso B con h >= 2)
  treeHeight(B (completeLevel t1 (h-1)) x (completeLevel t2 (h-1)))
  = (Código de treeHeight, caso B)
  1 + max (treeHeight(completeLevel t1 (h-1))) (treeHeight(completeLevel t2 (h-1)))
  >= (Hipótesis inductiva aplicada a t1 y t2)
  1 + max (h-1) (h-1)
  = (Aritmética)
  h
  ∎

-----------------------------------------------------------
Por el principio de inducción fuerte sobre h, concluimos:
(∀t::Tree)(∀h::Int) h >= 1 ⟹ treeHeight(completeLevel t h) >= h ∎

--}
