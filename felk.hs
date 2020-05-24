{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

bhsum :: Int -> Int
bhsum 1 = 1
bhsum n = n + bhsum (n - 1)

bhsum2 :: Int -> Int -> Int
bhsum2 a b 
    | a == b    = a
    | otherwise = b + bhsum2 a (b - 1)

create_list :: Int -> [Int]
create_list n = go 1 n
    where
        go :: Int -> Int -> [Int]
        go a n 
            | a == n    = [n]
            | otherwise = a : go (a + 1) n

reverse_create :: (Integral a, Eq a, Ord a) => a -> [a]
reverse_create x
    | x <= 1    = [1]
    | otherwise = x : reverse_create (x - 1)

naturals :: Int -> IO ()
naturals n = go 1 n
    where 
        go :: Int -> Int -> IO ()
        go x n
            | x == n    = putStrLn . show $ x
            | otherwise = do
                            putStrLn . show $ x
                            go (x + 1) n

evens :: Int -> IO ()
evens n = go 2 n
    where
        go :: Int -> Int -> IO ()
        go x n
            | x == n && even x  = putStrLn . show $ x
            | x == n && odd x   = return ()
            | even x            = do
                                    putStrLn . show $ x
                                    go (x + 1) n
            | odd x             = go (x + 1) n

type Database a b = [(a, b)]

new :: Database a b
new = []

write :: Eq key => (Database key value) -> key -> value -> (Database key value)
write db k v = go db k v []
    where 
        go :: Eq k => Database k v -> k -> v -> Database k v -> Database k v
        go [] k v acc = (k, v):acc
        go (x@(nk, nv):xs) k v acc
            | nk == k   = go xs k v acc
            | otherwise = go xs k v (x:acc)
            
delete :: Eq key => Database key value -> key -> Database key value
delete [] key = []
delete (x@(k, v):xs) key
    | k == key  = delete xs key
    | otherwise = x : delete xs key

delete' :: Eq k => Database k v -> k -> Database k v
delete' db k = filter (\(key, value) -> key /= k) db

readdb :: Eq key => Database key value -> key -> Either value (String, String)
readdb [] k = Right ("error", "not found")
readdb ((k, v):xs) key
    | k == key  = Left v
    | otherwise = readdb xs key

match :: Eq value => Database key value -> value -> [key]
match [] value = []
match ((k, v):xs) value
    | v == value    = k:match xs value
    | otherwise     = match xs value

bhfilter :: [Int] -> Int -> [Int]
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
concatenate ([]:xss) = concatenate xss
concatenate ((x:xs):xss) = x:concatenate (xs:xss)

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = concatenate [first x xs, [x], last x xs]
    where
        first :: Int -> [Int] -> [Int]
        first x xs = quicksort (filter (< x) xs)

        last :: Int -> [Int] -> [Int]
        last  x xs = quicksort (filter (>= x) xs)

mergesort :: (Eq a, Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort . take n $ xs) (mergesort . drop n $ xs)
    where
        n = div (length xs) 2

        merge :: (Eq a, Ord a) => [a] -> [a] -> [a]
        merge [] bs = bs
        merge as [] = as
        merge x@(a:as) y@(b:bs)
            | a <= b    = a : merge as y
            | otherwise = b : merge x  bs

bhtakeWhile :: (a -> Bool) -> [a] -> [a]
bhtakeWhile pred [] = []
bhtakeWhile pred (x:xs)
    | pred x    = x : bhtakeWhile pred xs
    | otherwise = []

bhdropWhile :: (a -> Bool) -> [a] -> [a]
bhdropWhile pred [] = []
bhdropWhile pred a@(x:xs)
    | pred x    = bhdropWhile pred xs
    | otherwise = a

bhiterate :: (a -> a) -> a -> [a]
bhiterate f a = a : bhiterate f (f a)

bhall :: (a -> Bool) -> [a] -> Bool
bhall pred [] = True
bhall pred (x:xs)
    | pred x    = bhall pred xs
    | otherwise = False

bhany :: (a -> Bool) -> [a] -> Bool
bhany pred [] = False
bhany pred (x:xs)
    | not . pred $ x    = bhany pred xs
    | otherwise         = True 

deleteAll :: Eq a => a -> [a] -> [a]
deleteAll x xs = filter (/=x) xs

countAll :: Eq a => a -> [a] -> Int
countAll x = length . filter (==x)

histogram :: Eq a => [a] -> [(a, Int)]
histogram = foldr fold []
    where
        fold :: Eq a => a -> [(a, Int)] -> [(a, Int)]
        fold a [] = [(a, 1)]
        fold a (x@(b, c):xs)
            | a == b    = (b, (c + 1)):xs
            | otherwise = x : fold a xs

sort :: Ord a=> [(a, Int)] -> [(a, Int)]
sort [] = []
sort ((x, a):xs) = concatenate [first x xs, [(x, a)], last x xs]
    where
        first x xs = sort (filter (\(a, b) -> a < x) xs)
        last  x xs = sort (filter (\(a, b) -> a >= x) xs)

permInv :: [Int] -> [Int]
permInv = snd . unzip . sort . flip zip [1..]

indSorted :: Ord a => [a] -> [Int]
indSorted = snd . unzip . sort . flip zip [1..]

bhminimum :: Ord a => [a] -> a
bhminimum (x:xs) = foldr (\a b -> if a < b then a else b) x xs

bhSum :: Num a => [a] -> a
bhSum = foldr (+) 0

bhAnd :: [Bool] -> Bool
bhAnd = foldr (&&) True

bhconcat :: [[a]] -> [a]
bhconcat = foldr (++) []

(~~) :: [a] -> [a] -> [a]
(~~) = flip (foldr (:))

bhlength :: [a] -> Int
bhlength = foldr (\a b -> b + 1) 0

bhReverse :: [a] -> [a]
bhReverse = foldl (flip (:)) []

isort :: (Ord a) => [a] -> [a]
isort = foldr sort []
    where 
        sort :: (Ord a) => a -> [a] -> [a]
        sort n [] = [n]
        sort n a@(x:xs)
            | n < x     = n:a
            | otherwise = x:sort n xs

fromPoint :: Num a => a -> a -> a -> a
fromPoint x a1 a0 = (x * a1) + a0

fromBinary :: [Int] -> Int
fromBinary = foldl (fromPoint 2) 0

polynomial :: [Int] -> Int -> Int
polynomial p a = foldl (fromPoint a) 0 p

sums :: [Int] -> [Int]
sums (x:xs) = scanl (+) x xs

fibs :: [Int]
fibs = 1 : scanl (+) 1 fibs

type BHString = [Char]
type Year = Int
type Chess = (Char, Int)

type DoubleList a = [[a]]
type Point a    = (a, a)

data Bit = Zero
         | One
         deriving (Eq, Ord, Show)

type Binary = [Bit]

instance {-# OVERLAPPING #-} Show Binary where
    show xs = go xs "[" "" "]"
        where
            go :: Binary -> String -> String -> String -> String
            go [] b acc e = b ++ acc ++ e
            go (bit:[]) b acc e = go [] b ((show bit) ++ acc) e
            go (bit:bits) b acc e = go bits b (", " ++ (show bit) ++ acc) e

plus1 :: Binary -> Binary
plus1 [] = [One]
plus1 (Zero:xs) = One:xs
plus1 (One:xs) = Zero:plus1 xs

add :: Binary -> Binary -> Binary
add xs [] = xs
add [] ys = ys
add (One:xs) (One:ys) = Zero : add (plus1 xs) ys
add (Zero:xs) (Zero:ys) = Zero : add xs ys
add (_:xs) (_:ys) = One : add xs ys

data Tern = ZERO
          | ONE
          | TWO
          deriving (Eq, Ord, Show)

type Ternary = [Tern]

instance {-# OVERLAPPING #-} Show Ternary where
    show xs = go xs "[" "" "]"
        where
            go :: Ternary -> String -> String -> String -> String
            go [] b acc e = b ++ acc ++ e
            go (bit:[]) b acc e = go [] b ((show bit) ++ acc) e
            go (bit:bits) b acc e = go bits b (", " ++ (show bit) ++ acc) e

plus1t :: Ternary -> Ternary
plus1t [] = [ONE]
plus1t (ZERO:xs) = ONE:xs
plus1t (ONE:xs)  = TWO:xs
plus1t (TWO:xs)  = ZERO:plus1t xs

addt :: Ternary -> Ternary -> Ternary
addt xs [] = xs
addt [] xs = xs
addt (TWO:xs) (TWO:ys) = ONE : addt (plus1t xs) ys
addt (ONE:xs) (TWO:ys) = ZERO : addt (plus1t xs) ys
addt (TWO:xs) (ONE:ys) = ZERO : addt (plus1t xs) ys
addt (ONE:xs) (ONE:ys) = TWO : addt xs ys
addt (ZERO:xs) (ZERO:ys) = ZERO : addt xs ys
addt (_:xs) (_:ys) = ONE : addt xs ys
