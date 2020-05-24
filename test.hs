removeAt 0 (x:xs) = xs
removeAt n []     = []
removeAt n (x:xs) = x : removeAt (n-1) xs

data Point = Point { x :: Double
                   , y :: Double
                   , visible :: Bool
                   }
                   deriving Show

origo = Point { x = 0
              , y = 0
              , visible = True
              }

data Q = Q { nom :: Int
           , den :: Int
           }

simplify :: Q -> Q

mkQ :: Int -> Int -> Q

instance Show Q where
    show q = show (nom q) ++ "/" ++ show (den q)

instance Eq Q where
    a == b = (nom a == nom b) && (den a == den b)


