gcd' a b | a == b = a
	 | a < b = gcd' b a
	 | a > b =
		if r == 0 then b else b `gcd'` r
		where r = a `mod` b

lcm' x y =  (x * y) `div` (x `gcd'` y)
