import Person

tst = 1

tt1 :: Int -> Int
tt1 x = x

tt2 :: Int -> String -> String
tt2 x y = y

data Vector a = Vector a a a deriving (Show)

vplus :: Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector l m n
