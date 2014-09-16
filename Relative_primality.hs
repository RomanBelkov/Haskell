gcd' a b | a == b = a
		 | a < b = gcd' b a
		 | a > b =
			if r == 0 then b else b `gcd'` r
			where r = a `mod` b

rprim x y = if gcd' x y == 1 then "relatively prime" else "not relatively prime"