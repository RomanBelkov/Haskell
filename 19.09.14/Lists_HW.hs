invert' []       = []
invert' (x : xs) = (invert' xs) ++ [x]

map' f []       = []
map' f (x : xs) = (f x) : map' f xs 

zip' (x : xs) (y : ys) = (x , y) : zip xs ys
zip' _        _        = []

unzip' []            = ([], [])
unzip' ((x, y) : as) = (x : (fst rest), y : (snd rest))
    where rest = unzip' as

flatten []       = []
flatten (x : xs) = x ++ (flatten xs)