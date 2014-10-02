gcd' a b = (\ r -> if r then b else gcd' b r) $ mod a b -- $ - операция применения

--prim n = prim' 2 where
--    prim' d | d * d > n = True
--            | n `mod` d == 0 = False
--            | otherwise = prim' (d + 1)

--only important functons at the top!
--no n inside prim'
-- || and && are not strict

prim n = prim' 2 where
    prim' d = d * d > n || (n `mod` d /= 0) && prim' (d + 1)

bor a b = if a then True else b