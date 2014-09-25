rnorm (_, 0) = error "division by zero"
rnorm (0, x) = (0, 1)
rnorm (a, b) = (a `div` g, b `div` g) where g = gcd a b

radd (a, b) (c, d) = rnorm (a * d + c * b , b * d)

rsub (a, b) (c, d) = radd (a, b) (-c, d)

rinv (a, b) = (b, a)

rmul (a, b) (c, d) = rnorm (a * c, b * d)

rdiv (a, b) (c, d) = (a, b) `rmul` rinv (c, d)