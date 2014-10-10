gcd' a b | a == b = a
         | a < b = gcd' b a
         | a > b =
	     	if r == 0 then b else b `gcd'` r
			where r = a `mod` b

phi 1 = 1
phi x = loop 2 1 x 
	where loop a acc x | x == a = acc
                       | x `gcd'` a == 1 = loop (a + 1) (acc + 1) x
                       | otherwise = loop (a + 1) acc x

phi' x = [m | m <- [1..x], gcd m x == 1]