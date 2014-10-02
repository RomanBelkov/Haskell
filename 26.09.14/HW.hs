foldl' f a []       = a
foldl' f a (x : xs) = foldl' f (f a x) xs

foldr' f a []       = a
foldr' f a (x : xs) = f x (foldr' f a xs)

max' l = foldl max (head l) l
min' l = foldl min (head l) l

divisors x = 1 : [ y | y <- [2..(x `div` 2)], x `mod` y == 0] ++ [x]
isPrim x = divisors x == [1, x] 

primNums = [x | x <- [2..], isPrim x]

fibNums = 1 : 1 : zipWith (+) fibNums (tail fibNums)