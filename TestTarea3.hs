import Tarea3 (Tree(..), unir, producto, cumplen, descartar, t, sumTree, treeToList, treeHeight, completeLevel)

-- Función simple para testear
assert :: (Show a, Eq a) => String -> a -> a -> IO ()
assert name expected actual =
    if expected == actual
    then putStrLn $ "✓ PASS: " ++ name
    else putStrLn $ "✗ FAIL: " ++ name ++ "\n  Expected: " ++ show expected ++ "\n  Got:      " ++ show actual

-- Tests para Ejercicio 1
testUnir :: IO ()
testUnir = do
    putStrLn "\n=== Ejercicio 1: unir ==="
    assert "unir [2,8] [0,9]" [2,8,0,9] (unir [2,8] [0,9])
    assert "unir [] [8,11]" [8,11] (unir [] [8,11])
    assert "unir [1,2,3] []" [1,2,3] (unir [1,2,3] [])

testProducto :: IO ()
testProducto = do
    putStrLn "\n=== Ejercicio 1: producto ==="
    assert "producto []" 1 (producto [])
    assert "producto [1,3,4]" 12 (producto [1,3,4])
    assert "producto [2,5]" 10 (producto [2,5])

-- Tests para Ejercicio 2
testCumplen :: IO ()
testCumplen = do
    putStrLn "\n=== Ejercicio 2: cumplen ==="
    assert "cumplen even [2,3,5,7,8,9,11,12]" [2,8,12]
        (cumplen even [2,3,5,7,8,9,11,12])
    assert "cumplen (>2) [4,2,-3,1,5,6]" [4,5,6]
        (cumplen (>2) [4,2,-3,1,5,6])

testDescartar :: IO ()
testDescartar = do
    putStrLn "\n=== Ejercicio 2: descartar ==="
    assert "descartar even [2,3,5,7,8,9,11,12]" [3,5,7,9,11]
        (descartar even [2,3,5,7,8,9,11,12])
    assert "descartar (>2) [4,2,-3,1,5,6]" [2,-3,1]
        (descartar (>2) [4,2,-3,1,5,6])

testLengthProperty :: IO ()
testLengthProperty = do
    putStrLn "\n=== Ejercicio 2: Propiedad length ==="
    let xs = [2,3,5,7,8,9,11,12]
    let p = even
    let lengthSum = length (cumplen p xs) + length (descartar p xs)
    assert "length(cumplen p xs) + length(descartar p xs) = length xs"
        (length xs) lengthSum

-- Tests para Ejercicio 3
testSumTree :: IO ()
testSumTree = do
    putStrLn "\n=== Ejercicio 3: sumTree ==="
    assert "sumTree t" 69 (sumTree t)

testTreeToList :: IO ()
testTreeToList = do
    putStrLn "\n=== Ejercicio 3: treeToList ==="
    assert "treeToList t" [4,8,9,14,16,18] (treeToList t)

testTreeHeight :: IO ()
testTreeHeight = do
    putStrLn "\n=== Ejercicio 3: treeHeight ==="
    assert "treeHeight t" 4 (treeHeight t)
    assert "treeHeight (L 5)" 1 (treeHeight (L 5))
    assert "treeHeight (U 5 (L 3))" 2 (treeHeight (U 5 (L 3)))
    assert "treeHeight (B (L 1) 2 (L 3))" 2 (treeHeight (B (L 1) 2 (L 3)))

testCompleteLevel :: IO ()
testCompleteLevel = do
    putStrLn "\n=== Ejercicio 3: completeLevel ==="

    -- Test 1: Caso base h=1
    let t1 = L 5
    let result1 = completeLevel t1 1
    assert "completeLevel (L 5) 1 mantiene altura 1" 1 (treeHeight result1)

    -- Test 2: Crear árbol de altura 3
    let result2 = completeLevel (L 5) 3
    assert "completeLevel (L 5) 3 crea altura 3" 3 (treeHeight result2)

    -- Test 3: Mantiene altura cuando h = treeHeight t
    let result3 = completeLevel t (treeHeight t)
    assert "completeLevel t (treeHeight t) mantiene altura"
        (treeHeight t) (treeHeight result3)

    -- Test 4: Árbol incompleto se completa
    let t4 = U 5 (L 3)
    let h4 = treeHeight t4
    let result4 = completeLevel t4 h4
    assert "completeLevel de árbol incompleto mantiene altura"
        h4 (treeHeight result4)

-- Main que corre todos los tests
main :: IO ()
main = do
    putStrLn "╔═══════════════════════════════════════════════════╗"
    putStrLn "║  Tests Unitarios - Tarea 3: Listas y Árboles    ║"
    putStrLn "╚═══════════════════════════════════════════════════╝"

    testUnir
    testProducto
    testCumplen
    testDescartar
    testLengthProperty
    testSumTree
    testTreeToList
    testTreeHeight
    testCompleteLevel

    putStrLn "\n╔═══════════════════════════════════════════════════╗"
    putStrLn "║  Tests Completados                               ║"
    putStrLn "╚═══════════════════════════════════════════════════╝"
