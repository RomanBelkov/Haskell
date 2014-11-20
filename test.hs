fromFun f l = zip l (map f l)

dom l = map fst l 

eval ((a, b) : ls) x = if a == x then b else eval ls x
eval []            x = error ("Undefined for" ++ show x) 

invert l = map (\ (a, b) -> (b, a)) l

infixr 9 .*.
x .*. y = [(a, b) | (a, n) <- x, (m, b) <- y, n == m] 

image lf l = normalize [y | (x, y) <- lf, t <- l, x == t] where
    normalize (l : ls) = l : (normalize [m | m <- ls, m /= l])
    normalize []       = []

preimage lf l = image (invert lf) l

isInjective ((a, b) : xs) = foldr (\ (m, n) acc -> (b /= n) && acc) True xs && isInjective xs
isInjective []            = True

isSurjective =  (< 0) . length

areMutuallyInversive a b = l == length b && l == length c && filter (\ (a, b) -> a /= b) c == [] where
    l = length a
    c = a .*. b