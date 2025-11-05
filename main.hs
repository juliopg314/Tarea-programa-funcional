-- Definición de la función 'quicksort' 
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = quicksort menores ++ [p] ++ quicksort mayores
    where
        menores = filter (<= p) xs
        mayores = filter (> p) xs

-- Punto de entrada 'main' 
-- 'main' es la acción que se ejecuta al correr el programa.
-- Su tipo 'IO ()' indica que realiza acciones de Input/Output (como imprimir).
main :: IO ()
main = do
    -- Definimos una lista de ejemplo
    let listaOriginal = [7, 2, 9, 1, 5, 10, 3]
    
    -- Llamamos a nuestra función 'quicksort' con esa lista
    let listaOrdenada = quicksort listaOriginal
    
    -- Usamos 'print' para imprimir el resultado en la consola
    print listaOrdenada

