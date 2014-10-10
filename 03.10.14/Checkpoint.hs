sprod x y = foldl (+) 0 (zipWith (*) x y)

normalize (l : ls) = l : (normalize [m | m <- ls, m /= l])
normalize []       = []

sort (l : ls) = (sort [m | m <- ls, m <= l]) ++ [l] ++ (sort [m | m <- ls, m > l])
sort _        = []

isProgression []  = True
isProgression [_] = True
isProgression l   = (length l - 1) == (length ([m | m <- zs, m == z]) + 1)
    where (z : zs) = zipWith (-) l (tail l)

isFunction [] = True
isFunction r  = t == normalize t where t = map fst r

isSymmetric ((x, y) : xs) | x == y    = isSymmetric xs
                          | otherwise = contains (y, x) xs && isSymmetric (filter (/= (y,x) ) xs) 
                              where contains _ []     = False
                                    contains x (y:ys) = x == y || contains x ys
isSymmetric []            = True


isReflexive r = length all == (length [m | m <- all, contains (m, m) r])
    where all               = normalize (map fst r ++ map snd r)
          contains _ []     = False
          contains x (y:ys) = x == y || contains x ys