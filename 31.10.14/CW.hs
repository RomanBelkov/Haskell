import List

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

data Lambda' = 
      Var' Int        
    | App' Lambda' Lambda' 
    | Lam' Int Lambda'
        deriving (Show)

br (Var x)   acc = findIndex 0 acc
br (App x y) acc = App' (br x acc) (br y acc)
br (Lam x y) acc = Lam' (br y (x : acc))

shift = shift' 0 where
        shift' n (App a b) = App (shift' n a) (shift' n b)
        shift' n (Lam a)   = Lam $ shift' (n + 1) a