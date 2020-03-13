--Bodoki-Halmen Zsolt
--bzim1700
--531/1

--1
bhtakeWhile :: (a -> Bool) -> [a] -> [a]
bhtakeWhile pred [] = []
bhtakeWhile pred (x:xs)
    | pred x    = x:bhtakeWhile pred xs
    | otherwise = []

bhdropWhile :: (a -> Bool) -> [a] -> [a]
bhdropWhile pred [] = []
bhdropWhile pred a@(x:xs)
    | pred x    = bhdropWhile pred xs
    | otherwise = a

bhiterate :: (a -> a) -> a -> [a]
bhiterate func a = a:bhiterate func (func a)

bhall :: (a -> Bool) -> [a] -> Bool
bhall pred [] = True
bhall pred (x:xs)
    | pred x    = bhall pred xs
    | otherwise = False

bhany :: (a -> Bool) -> [a] -> Bool
bhany pred [] = False
bhany pred (x:xs)
    | pred x    = True
    | otherwise = bhany pred xs

--2
deleteAll :: Eq a => a -> [a] -> [a]
deleteAll a = filter (/= a)

countAll :: Eq a => a -> [a] -> Int
countAll a = length . filter (== a)

histogram :: Eq a => [a] -> [(a, Int)]
histogram = foldr hist []
    where
        hist :: Eq a => a -> [(a, Int)] -> [(a, Int)]
        hist a [] = [(a, 1)]
        hist a ((x, n):xs)
            | a == x    = (x, n+1):xs
            | otherwise = (x, n):hist a xs

sort :: Ord a => (a, Int) -> [(a, Int)] -> [(a, Int)]
sort x [] = [x]
sort (a, b) ((x, n):xs)
    | a < x     = (a, b):(x, n):xs
    | a > x     = (x, n):sort (a, b) xs

permInv :: [Int] -> [Int]
permInv = snd . unzip . foldr sort [] . flip zip [1..]

indSorted :: Ord a => [a] -> [Int]
indSorted = snd . unzip . foldr sort [] . flip zip [1..]
