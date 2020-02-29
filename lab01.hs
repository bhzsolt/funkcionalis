--1
bhsum :: (Num a, Eq a, Ord a) => a -> a
bhsum n
    | n <= 0    = n
    | otherwise = n + bhsum (n-1)

bhsum2 :: (Num a, Eq a, Ord a) => a -> a -> a
bhsum2 n m 
    | n >  m    = bhsum2 m n
    | n == m    = n
    | otherwise = n + bhsum2 (n+1) m

--2
create_list :: (Integral a, Eq a, Ord a) => a -> [a]
create_list x = go x []
    where
        go :: (Integral a, Eq a, Ord a) => a -> [a] -> [a]
        go x xs
            | x <= 1    = 1:xs
            | otherwise = go (x-1) (x:xs)

reverse_create :: (Integral a, Eq a, Ord a) => a -> [a]
reverse_create x
    | x <= 1    = [1]
    | otherwise = x:reverse_create (x-1)

--3
naturals :: (Integral a, Ord a, Show a) => a -> IO ()
naturals x = go 1 x
    where
        go :: (Integral a, Ord a, Show a) => a -> a -> IO ()
        go x y 
            | x >= y    = putStrLn . show $ x
            | otherwise = do
                            putStrLn . show $ x
                            go (x+1) y 

evens :: (Integral a, Ord a, Show a) => a -> IO ()
evens x = go 0 x
    where
        go :: (Integral a, Ord a, Show a) => a -> a -> IO ()
        go x y
            | x >= y && odd x = putStr ""
            | even x    = do
                            putStrLn . show $ x
                            go (x+1) y
            | otherwise = go (x+1) y

--4
new :: (Eq a, Eq b) => [(a, b)]
new = []

write :: (Eq a, Eq b) => [(a, b)] -> a -> b -> [(a, b)]
write db key value = go db [] key value
    where
        go :: (Eq a, Eq b) => [(a, b)] -> [(a, b)] -> a -> b -> [(a, b)]
        go [] acc key value = (key, value):acc
        go (d@(k, v):db) acc key value
            | k == key  = go db acc key value
            | otherwise = go db (d:acc) key value

delete :: (Eq a, Eq b) => [(a, b)] -> a -> [(a, b)]
delete [] key = []
delete (d@(k, v):db) key
    | k == key  = delete db key
    | otherwise = d:delete db key

dbread :: (Eq a, Eq b) => [(a, b)] -> a -> Either (String, String) (String, b)
dbread [] key = Left ("error", "not found")
dbread ((k,v):db) key
    | k == key  = Right ("ok", v)
    | otherwise = dbread db key

match :: (Eq a, Eq b) => [(a, b)] -> b -> [a]
match [] value = []
match ((k, v):db) value
    | v == value    = k:match db value
    | otherwise     = match db value

--5
bhfilter :: Ord a => [a] -> a -> [a]
bhfilter [] _ = []
bhfilter (x:xs) n
    | x < n     = x:bhfilter xs n
    | otherwise = bhfilter xs n

bhreverse :: [a] -> [a]
bhreverse xs = go xs []
    where
        go :: [a] -> [a] -> [a]
        go [] acc = acc
        go (x:xs) acc = go xs (x:acc)

concatenate :: [[a]] -> [a]
concatenate [] = []
concatenate ([]:xs) = concatenate xs
concatenate ((x:xs):xss) = x:concatenate (xs:xss)

--6
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = go x xs
    where
        go :: Ord a => a -> [a] -> [a]
        go x xs = concatenate [low, [x], high]
            where 
                low = quicksort . filt (\x n -> x < n) xs $ x
                high = quicksort . filt (\x n -> x >= n) xs $ x

                filt :: Ord a => (a -> a -> Bool) -> [a] -> a -> [a]
                filt _ [] _ = []
                filt pred (x:xs) n
                    | pred x n  = x:filt pred xs n
                    | otherwise = filt pred xs n



mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort . chunk take $ xs) (mergesort . chunk drop $ xs)
    where
        chunk :: (Ord a) => (Int -> [a] -> [a]) -> [a] -> [a]
        chunk func xs = func (length xs `div` 2) xs

        merge :: (Ord a) => [a] -> [a] -> [a]
        merge [] [] = []
        merge [] xs = xs
        merge xs [] = xs
        merge a@(x:xs) b@(y:ys)
            | x <= y    = x:merge xs b
            | otherwise = y:merge a ys
