fact 0 = 1
fact x = x * fact (x - 1)

prim x = if fact (x - 1) `mod` x == (x - 1) then "prime" else "non-prime"