data Lambda = 
      Var String        
    | App Lambda Lambda 
    | Lam String Lambda

instance Show Lambda where
    show (Var x)   = x
    show (Lam x y) = "\\" ++ x ++ show y
    show (App x y) = (lamX $ show x) ++ (appY $ show y) where
            lamX = case x of Lam _ _ -> br; _ -> id
            appY = case y of App _ _ -> br; _ -> id
            br x = "(" ++ x ++ ")"

fv (Var x)   = [x]
fv (App x y) = (fv x) ++ (fv y)
fv (Lam x y) = filter (/= x) (fv y)

subst t x b = sub t where
    sub e@(Var v) = if v == x then b else e
    sub (App m n) = App (sub m) (sub n)
    sub (Lam v m) = 
        if x == v then 
            Lam v m 
        else if v `elem` fv b then
                 let k = newVar m v
                     f = substVar m v k
                 in Lam k (sub f)  
             else 
                 Lam v (sub m)
    newVar e i = loop i
           where loop i' = if i' `elem` vars then loop (i ++ "'") else i'
                 vars = fv b ++ fv e
    substVar e s s' = subst e s (Var s')

red x = rd x [] where 
    rd (App f a) as = rd f (a:as)
    rd (Lam s e) [] = Lam s (red e)
    rd (Lam s e) (a : as) = rd (subst e s a) as
    rd f as = foldl App f (map red as)

eq (Var v)   (Var v')    = v == v'
eq (App m n) (App m' n') = eq m m' && eq n n'
eq (Lam m n) (Lam m' n') = eq n (substVar n' m' m)
    where substVar e s s' = subst e s (Var s')
eq _         _ = False
