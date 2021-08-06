doubleNumbers x = case x of
    (x:(y:zs))  -> if even $length zs  then x * 2: y : doubleNumbers zs 
                                       else x : y * 2 : doubleNumbers zs
    [x]         ->  [x]
    []          ->  []
sumDigits x
  | div x 10 > 0 = mod x 10 + sumDigits (div x 10)
  | otherwise    = x
sumAllDigits x = sum $ map sumDigits $ doubleNumbers x
divTen x = mod (sumAllDigits x) 10 
numToArray x 
  | div x 10 > 0 = mod x 10 : numToArray (div x 10)
  | otherwise    = [x]
validate x = divTen (reverse $ numToArray x) == 0
