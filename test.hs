removeAt 0 (x:xs) = xs
removeAt n []     = []
removeAt n (x:xs) = x : removeAt (n-1) xs
