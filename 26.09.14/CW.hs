unzip'' l = (map fst l, map snd l) --bad, map calls 2 times

unzip''' []       = ([], [])
unzip''' (x : xs) = (fst x : fst t, snd x : snd t) where t = unzip''' xs
--bad, pattern matching is done 2 times

unzip' []            = ([], [])
unzip' ((x, y) : as) = (x : rx, y : ry)
    where (rx, ry) = unzip' as
--OK

invert' []       = []
invert' (x : xs) = (invert' xs) ++ [x]
--bad

invert'' acc (x : xs) = invert'' (x : acc) (xs)
invert'' acc []       = acc

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