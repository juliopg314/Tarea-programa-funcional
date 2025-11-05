
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = quicksort menores ++ [p] ++ quicksort mayores
    where
        menores = filter (<= p) xs
        mayores = filter (> p) xs

main :: IO ()
main = do
    
    let listaOriginal = [7, 2, 9, 1, 5, 10, 3]
    
    let listaOrdenada = quicksort listaOriginal
    
   
    print listaOrdenada
