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
unir l1 [] = l1
unir [] l2 = l2
unir l1 l2 = l1 ++ l2

--2)
producto :: [Int] -> Int
producto [] = 0
producto (x:[]) = x
producto (x:xs) = x * producto xs

--3)
{--
(∀ l1,l2::[Int]) producto(unir l1 l2) = (producto l1)*(producto l2)

-}

--------------------
----Ejercicio 2:----
--------------------
--1)
cumplen :: (a -> Bool) -> [a] -> [a]
cumplen p xs = undefined

--2)
descartar :: (a -> Bool) -> [a] -> [a]
descartar p xs = undefined

--3)
{--
(∀ xs::[a]) (∀p::(a− > Bool)) length(cumplen p xs) + length(descartar p xs) = length xs

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
