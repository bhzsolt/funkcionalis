{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

--Bodoki-Halmen Zsolt
--bzim1700
--531/1

--1

type BHString = [Char]
type Year = Int
type Chess = (Char, Int)

--2
type List a = [[a]]
type Point a = (a, a)

--3
data Answer = Yes
            | No
            | Maybe
            deriving Show

andAnswer :: Answer -> Answer -> Answer
andAnswer No _ = No
andAnswer _ No = No
andAnswer Maybe _ = Maybe
andAnswer Yes x = x

orAnswer :: Answer -> Answer -> Answer
orAnswer Yes _ = Yes
orAnswer _ Yes = Yes
orAnswer Maybe _ = Maybe
orAnswer No x = x

data Directions = East
                | South
                | West
                | North
                deriving Show

toLeft :: Directions -> Directions
toLeft East = North
toLeft South = East
toLeft West = South
toLeft _ = West

toRight :: Directions -> Directions
toRight East = South
toRight South = East
toRight West = South
toRight _ = West

toBack :: Directions -> Directions
toBack East = West
toBack South = North
toBack West = East
toBack _ = South

data Bit = Zero
         | One
         deriving Show

type Binary = [Bit]

instance {-# OVERLAPPING #-} Show Binary where
    show xs = go xs "[" [] "]"
        where
            go :: Binary -> String -> String -> String -> String
            go [] b acc e = b ++ acc ++ e
            go (bit:[]) b acc e = go [] b ((show bit)++acc) e
            go (bit:xs) b acc e = go xs b (("," ++ show bit) ++ acc) e

plus1 :: Binary -> Binary
plus1 [] = [One]
plus1 (Zero:xs) = One:xs
plus1 (One:xs) = Zero:plus1 xs

add :: Binary -> Binary -> Binary
add [] [] = []
add [] x = x
add x [] = x
add (One:xs) (One:ys) = Zero:add (plus1 xs) ys
add (Zero:xs) (Zero:ys) = Zero:add xs ys
add (_:xs) (_:ys) = One:add xs ys

--4
safeDiv :: Integral a => a -> a -> Maybe a
safeDiv x 0 = Nothing
safeDiv x y = Just (div x y)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

data Tuple a b = Tuple a b
               deriving Show

data Triple a b c = Triple a b c
                  deriving Show

--5
data Array a = Empty
             | Leaf a
             | Array (Array a, Array a, Array a, Array a)
             deriving Eq

instance Show a => Show (Array a) where
    show Empty = "Empty"
    show (Leaf x) = show x
    show (Array (x0, x1, x2, x3)) = concat ["Array ("
                                           , show x0
                                           , ", "
                                           , show x1
                                           , ", "
                                           , show x2
                                           , ", "
                                           , show x3
                                           , ")"]

instance Functor Array where
    fmap f Empty = Empty
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Array (x0, x1, x2, x3)) = Array (fmap f x0, fmap f x1, fmap f x2, fmap f x3)

instance Foldable Array where
    foldr f e Empty                     = e
    foldr f e (Leaf x)                  = f x e
    foldr f e (Array (x0, x1, x2, x3))  = foldr f (foldr f (foldr f (foldr f e x3) x2) x1) x0


fromList :: [a] -> Array a
fromList xs
    | length xs == 0    = Empty
    | length xs <= 4    = fromSmallList xs
    | otherwise         = Array (fromList xs0, fromList xs1, fromList xs2, fromList xs3) 
        where
            (n0, n1, n2, n3) = portion (length xs) (0, 0, 0, 0)
            xs0 = take n0 xs
            xs1 = take n1 . drop n0             $ xs
            xs2 = take n2 . drop (n0 + n1)      $ xs
            xs3 = take n3 . drop (n0 + n1 + n2) $ xs

            as = fromSmallListHelper xs

            fromSmallListHelper :: [a] -> [Array a]
            fromSmallListHelper [] = [Empty]
            fromSmallListHelper (x:xs) = (Leaf x):fromSmallListHelper xs
            
            fromSmallList xs
                | length as >= 4    = Array (as !! 0, as !! 1, as !! 2, as !! 3)
                | length as == 3    = Array (as !! 0, as !! 1, as !! 2, Empty)
                | length as == 2    = Array (as !! 0, as !! 1, Empty, Empty)
                | length as == 1    = Array (as !! 0, Empty, Empty, Empty)
                | otherwise         = Empty

new :: a -> Int -> Array a
new a n 
    | n <= 0    = Empty
    | n == 1    = Array (Leaf a, Empty, Empty, Empty)
    | n == 2    = Array (Leaf a, Leaf a, Empty, Empty)
    | n == 3    = Array (Leaf a, Leaf a, Leaf a, Empty)
    | n == 4    = Array (Leaf a, Leaf a, Leaf a, Leaf a)
    | otherwise = Array (new a n0, new a n1, new a n2, new a n3)
        where
            (n0, n1, n2, n3) = portion n (0, 0, 0, 0)

portion :: Int -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
portion n x@(a, b, c, d)
    | n == 0    = x
    | n < 4     = go n x
    | (a-b) < 4 && (a-c) < 4 && (a-d) < 4 = portion (n-4) ((a+4), b, c, d)
    | (b-c) < 4 && (b-d) < 4 = portion (n-4) (a, (b+4), c, d)
    | (c-d) < 4 = portion (n-4) (a, b, (c+4), d)
    | otherwise = portion (n-4) (a, b, c, (d+4))
        where 
            go n (a, b, c, d)
                | (a-b) < 4 && (a-c) < 4 && (a-d) < 4 = ((a+n), b, c, d)
                | (b-c) < 4 && (b-d) < 4 = (a, (b+n), c, d)
                | (c-d) < 4 = (a, b, (c+n), d)
                | otherwise = (a, b, c, (d+n))

addIndexes :: [(a, Int)] -> a -> [(a, Int)]
addIndexes [] a = [(a, 0)]
addIndexes x@((b, n):xs) a = (a, n+1):x

peek :: Array a -> Int -> a
peek array index = fst . head . filter ((== index) . snd) . foldl addIndexes [] $ array

change :: Array a -> Int -> a -> Array a
change array index new = fromList . fst . unzip . changeIt i new . foldr (flip addIndexes) [] $ array
    where
        changeIt :: Int -> a -> [(a, Int)] -> [(a, Int)]
        changeIt index new [] = []
        changeIt index new ((x, i):xs)
            | index == i    = (new, i):xs
            | otherwise     = (x, i):changeIt index new xs

        i = (length array) - index - 1
