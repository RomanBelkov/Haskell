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
(P x) ||> (P y) = 


apply :: Pars s a -> s -> [(a, s)]
lift :: Pars s a -> (b -> Pars c a)
many :: Pars s a -> Pars s [a]
some :: Pars s a -> Pars s [a] -- 1 & more
opt :: Pars s a -> Pars s (Maybe a)
eof :: [(a, [c])] -> [a]
