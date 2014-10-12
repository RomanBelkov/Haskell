data Tsil a = Lin | Snoc (Tsil a) a

length' Lin        = 0
length' (Snoc a _) = 1 + length' a

map' f Lin        = Lin
map' f (Snoc a b) = Snoc (map' f a) (f b)

concat' Lin l = l
concat' 

reverse' acc (Snoc a b) = reverse' (Snoc acc b) a
reverse' acc Lin        = acc

--flatten
--toList
--fromList
