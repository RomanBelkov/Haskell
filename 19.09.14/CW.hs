sum [] = 0
sum' (x : xs) = sum' xs + x

prod' [] = 1
prod' (x : xs) = x * prod' xs

length' [] = 0
length' (_ : xs) = length' xs + 1

concat' [] s = s
concat' (x : xs) s = x : concat' xs s

invert' [] = []
invert' (x : xs) = (invert' xs) ++ [x]