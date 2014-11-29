newtype Pars s a = P (s -> [(a, s)])

failu = P (\ _ -> [])

anya = P f where
    f [] = [((), [])]
    f (_ : xs) = [((), xs)]

sym c = P f where
    f (x : xs) | c == x = [(c, xs)]
    f _        = []

val a = P (\ s -> [(a, s)])

infixl 2 |||
(P x) ||| (P y) = P (\t -> (x t) ++ (y t))

infixl 3 ||>
(P a) ||> f = P p where
    p s = concat [apply (f x) y | (x, y) <- a s]

apply (P p) = p

lift (P p) = \_ -> (P p)

lift' p _ = p

many a = val [] ||| a ||> (\x -> many a ||> (\y -> val (x : y)))

some a = a ||> (lift $ many a)

--some :: Pars s a -> Pars s [a] -- 1 & more
--opt :: Pars s a -> Pars s (Maybe a)
--eof :: [(a, [c])] -> [a]
