dc x = dc' 2 2 x
	where dc' a acc x | a * a > x = acc
			  | a * a == x = (acc + 1)
			  | x `mod` a == 0 = dc' (a + 1) (acc + 2) x
			  | otherwise = dc' (a + 1) acc x
