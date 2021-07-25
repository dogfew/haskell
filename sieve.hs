import Data.List
notDivisible y x = mod x y /= 0
sieve list =  filter (notDivisible current_prime) list ++ [current_prime]
                            where current_prime = head list
applyWhile f x = if head x ^ 2 < maximum x
                then applyWhile f (f x) 
                else  f x 
primesTo n = sort (1 : applyWhile sieve [2..n])
