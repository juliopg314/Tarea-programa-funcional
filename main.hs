-- Esta línea es la "firma de tipo". Es obligatoria en código formal.
-- 'quicksort' ::  -> Define una función llamada 'quicksort'.
-- (Ord a) =>      -> Es una "restricción". Significa: "para cualquier tipo 'a' (como Int o Char)
--                    siempre y cuando 'a' se pueda Ordenar (es decir, que entienda '<', '>', etc.)"
-- [a] -> [a]      -> "La función toma una lista de tipo 'a' ([a]) como entrada,
--                    y DEVUELVE una lista del mismo tipo 'a' ([a]) como salida."
quicksort :: (Ord a) => [a] -> [a]


-- CASO BASE (El punto de parada de la recursión)
-- Esta es la primera "definición" de la función, usando "Pattern Matching".
-- quicksort []      -> "SI la lista que recibe 'quicksort' está vacía (es '[]')..."
--              = [] -> "...entonces el resultado ES ('=') una lista vacía ('[]')."
-- Sin esto, la recursión sería infinita.
quicksort [] = []


-- CASO RECURSIVO (El "divide y vencerás")
-- Esta es la segunda "definición" de la función.
-- quicksort (p:xs) -> "SI la lista NO está vacía, la descomponemos en:"
-- (p:xs)           -> " 'p' (el pivote) será el primer elemento (la 'cabeza' de la lista)."
--                 -> " 'xs' será una lista con TODO el RESTO de elementos (la 'cola')."
-- =                -> "El resultado ES ('=') la siguiente concatenación:"
-- quicksort menores  -> "1. Llama recursivamente a 'quicksort' con la lista de 'menores'..."
-- ++ [p] ++         -> "2. Pega (concatena) el pivote 'p' (envuelto como una lista)..."
-- quicksort mayores  -> "3. Pega (concatena) el resultado de llamar recursivamente a 
--                      'quicksort' con la lista de 'mayores'."
quicksort (p:xs) = quicksort menores ++ [p] ++ quicksort mayores
    -- 'where' (donde) es una palabra clave para definir variables auxiliares
    -- que se usan en la línea de arriba.
    where
        -- 'menores = ...' -> Define la variable 'menores'.
        -- 'filter'        -> Es una función que toma una condición y una lista.
        -- '(<= p)'        -> Esta es la condición: "es menor o igual que (<=) el pivote 'p' ".
        -- 'xs'            -> Esta es la lista sobre la que se filtra (recuerda, 'xs' es el RESTO).
        -- Resultado: 'menores' es una NUEVA lista solo con los elementos de 'xs' <= 'p'.
        menores = filter (<= p) xs
        
        -- 'mayores = ...' -> Define la variable 'mayores'.
        -- 'filter (> p)'  -> La condición es: "es estrictamente mayor que (>) el pivote 'p' ".
        -- 'xs'            -> La lista a filtrar.
        -- Resultado: 'mayores' es una NUEVA lista solo con los elementos de 'xs' > 'p'.
        mayores = filter (> p) xs

-- Ejemplo visual del Caso Recursivo si la entrada es [5, 1, 9, 3]:
-- p  = 5
-- xs = [1, 9, 3]
-- 
-- menores = filter (<= 5) [1, 9, 3]  ->  [1, 3]
-- mayores = filter (> 5)  [1, 9, 3]  ->  [9]
-- 
-- Resultado = (quicksort [1, 3]) ++ [5] ++ (quicksort [9])
--           = [1, 3]             ++ [5] ++ [9]
--           = [1, 3, 5, 9]

-- Firma de tipo para 'main'. Siempre es 'IO ()' (o algo con 'IO').
-- 'main ::' -> "Define la acción principal 'main'."
-- 'IO ()'   -> "IO' significa que es una acción de 'Input/Output' (impura).
--            -> '()' (se llama "unit") significa que la acción no devuelve
--               ningún valor de cómputo, solo "hace algo" (el efecto de imprimir).
main :: IO ()
main = do
    -- 'do' -> Inicia un bloque de acciones 'IO' que se ejecutarán en secuencia.
    
    -- 'let' -> Dentro de un bloque 'do', 'let' se usa para definir una variable
    --          *pura* (normal, no de IO).
    -- 'listaOriginal = ...' -> Asigna la lista de números a la variable 'listaOriginal'.
    let listaOriginal = [7, 2, 9, 1, 5, 10, 3]
    
    -- 'let listaOrdenada = ...' -> Define otra variable pura.
    -- 'quicksort listaOriginal' -> Llama a nuestra función PURA. Haskell calcula
    --                            el resultado (la lista ordenada) y...
    -- '... = ...'             -> ...lo asigna a la variable 'listaOrdenada'.
    let listaOrdenada = quicksort listaOriginal
    
    -- 'print' -> Esta NO es una variable 'let'. Es una *acción de IO* pura.
    --            Le dice al sistema: "Toma el valor de 'listaOrdenada'
    --            y ejecúta el efecto secundario de mostrarlo en la consola".
    print listaOrdenada

--Perez Garcia Julio Roberto 
--se uso un compilador online https://www.onlinegdb.com/online_haskell_compiler
--referencias https://www.haskell.org/get-started/ "Programming in Haskell" de Graham Hutton.


