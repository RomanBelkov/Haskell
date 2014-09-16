ds x = ds' 2 1 x
	where ds' a acc x | a * a > x = acc + x
				      | a * a == x = acc + a + x
				      | x `mod` a == 0 = ds' (a + 1) (acc + a + x `div` a) x
				      | otherwise = ds' (a + 1) acc x
