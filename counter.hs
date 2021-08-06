counter :: (Eq a) => a -> [a] -> Integer
counter element x = subcounter element 0 x 
    where subcounter element number x = case x of 
            (x:xs) -> if x == element then subcounter element (number + 1) xs
                                      else subcounter element number       xs
            [x]    -> if x == element then number + 1 
                                      else number    
            []     -> number
