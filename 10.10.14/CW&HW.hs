data Tsil a = Lin | Snoc (Tsil a) a

length' Lin        = 0
length' (Snoc a _) = 1 + length' a

map' f Lin        = Lin
map' f (Snoc a b) = Snoc (map' f a) (f b)

foldl' f a Lin         = a
foldl' f a (Snoc xs x) = foldl' f (f a x) xs

foldr' f a Lin         = a
foldr' f a (Snoc xs x) = f x (foldr' f a xs)

concat' Lin         l = l
concat' (Snoc xs x) l = Snoc (xs `concat'` l) x

reverse' acc (Snoc xs x) = reverse' (acc `Snoc` x) xs
reverse' acc Lin         = acc

flatten' l = foldl' concat' Lin l

toList Lin         = []
toList (Snoc xs x) = x : (toList xs) 

toTsil []       = Lin
toTsil (x : xs) = Snoc (toTsil xs) x