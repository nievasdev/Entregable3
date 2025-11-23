module Main where

----------------------------------------------------
-- PARTE 1: TUS SOLUCIONES (Copia esto de tu tarea)
----------------------------------------------------

-- Ejercicio 1
unir:: [a] -> [a] -> [a]
unir l1 l2 = l1 ++ l2

producto :: [Int] -> Int
producto [] = 1
producto (x:xs) = x * producto xs

-- Ejercicio 2
cumplen :: (a -> Bool) -> [a] -> [a]
cumplen p [] = []
cumplen p (x:xs) = if p x then x : cumplen p  xs else cumplen p xs  

descartar :: (a -> Bool) -> [a] -> [a]
descartar p [] = []
descartar p (x:xs) = if p x then descartar p xs else x : descartar p xs

-- Ejercicio 3
data Tree = L Int | U Int Tree | B Tree Int Tree
 deriving (Show, Eq) -- Agregué Eq para poder comparar en los tests

t :: Tree
t = B (L 4) 8 (B (U 14 (L 9)) 16 (L 18))

sumTree :: Tree -> Int
sumTree (L x) = x
sumTree (U x t) = x + sumTree t
sumTree (B t1 x t2) = sumTree t1 + x + sumTree t2 

treeToList :: Tree -> [Int]
treeToList (L x) = [x]
treeToList (U x t) = treeToList t ++ [x]
treeToList (B t1 x t2) = treeToList t1 ++ [x] ++ treeToList t2

treeHeight :: Tree -> Int
treeHeight (L x) = 1
treeHeight (U x t) = 1 + treeHeight t
treeHeight (B t1 x t2) = 1 + max (treeHeight t1) (treeHeight t2)

completeLevel :: Tree -> Int -> Tree
completeLevel a 1 = a
completeLevel a 0 = a
completeLevel (L x) h = B (completeLevel (L x) (h-1)) x (completeLevel (L x) (h-1))
completeLevel (U x t) h = B (completeLevel t (h-1)) x (completeLevel (L x) (h-1))
completeLevel (B t1 x t2) h = B (completeLevel t1 (h-1)) x (completeLevel t2 (h-1))

----------------------------------------------------
-- PARTE 2: EL TEST (Para verificar que todo ande)
----------------------------------------------------

test :: (Eq a, Show a) => String -> a -> a -> IO ()
test nombre obtenido esperado = do
    putStr $ "Probando " ++ nombre ++ ": "
    if obtenido == esperado
        then putStrLn "OK"
        else putStrLn $ "FALLO. \n   Esperado: " ++ show esperado ++ "\n   Obtenido: " ++ show obtenido

main :: IO ()
main = do
    putStrLn "---- INICIANDO PRUEBAS (TODO EN UN ARCHIVO) ----"
    
    putStrLn "\n-- Ejercicio 1 --"
    test "unir" (unir [2,8] [0,9]) [2,8,0,9]
    test "producto" (producto [1,3,4]) 12

    putStrLn "\n-- Ejercicio 2 --"
    test "cumplen" (cumplen even [2,3,5,7,8,9,11,12]) [2,8,12]
    test "descartar" (descartar even [2,3,5,7,8,9,11,12]) [3,5,7,9,11]

    putStrLn "\n-- Ejercicio 3 --"
    test "sumTree" (sumTree t) 69
    test "treeToList" (treeToList t) [4,8,9,14,16,18]
    test "treeHeight" (treeHeight t) 4

    -- Prueba de completeLevel (verificando altura)
    let tCompleted = completeLevel t 4
    let hCompleted = treeHeight tCompleted
    putStr $ "Probando completeLevel (Altura): "
    if hCompleted == 4 then putStrLn "OK" else putStrLn ("FALLO, altura: " ++ show hCompleted)
    
    putStrLn "\n---- LISTO ----"
