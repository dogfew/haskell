isPrime n i    
        | i ^ 2 > n = True | z == 0 = False
        | z /= 0  = isPrime n (i + 1)
        where z = mod n i 
prime n = isPrime n 2
primes = filter prime [1..]
