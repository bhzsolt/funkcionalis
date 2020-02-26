--1
bhminimum :: Ord a => [a] -> a
bhminimum (x:xs) = foldr fold x xs
    where
        fold :: Ord a => a -> a -> a
        fold x min
            | x < min   = x
            | otherwise = min

bhsum :: Num a => [a] -> a
bhsum = foldr (+) 0

bhand :: [Bool] -> Bool
bhand = foldr (&&) True

bhconcat :: [[a]] -> [a]
bhconcat = foldr (++) []

(~~) :: [a] -> [a] -> [a]
(~~) = flip (foldr (:)) 

bhreverse :: [a] -> [a]
bhreverse = foldl (flip (:)) []

--2.1
isort :: Ord a => [a] -> [a]
isort = foldr sort []
    where
        sort :: Ord a => a -> [a] -> [a]
        sort x [] = [x]
        sort x (y:ys)
            | x < y     = x:y:ys
            | otherwise = y:sort x ys

--2.2
fromBinary :: [Integer] -> Integer
fromBinary = foldl fold 0
    where
        fold :: Integer -> Integer -> Integer
        fold b1 b0 = b1*2 + b0

--2.3
polynomial :: Num a => [a] -> a -> a
polynomial xs a = foldl (fold a) 0 xs
    where
        fold :: Num a => a -> a -> a -> a
        fold x a1 a0 = a1*x + a0

--2.4
sums :: Num a => [a] -> [a]
sums (x:xs) = scanl (+) x xs

--2.5
fibs :: [Integer]
fibs = 1:scanl (+) 1 fibs
