module CryptoMath(
    modPow,
    totient,
    distinctPrimes,
    getCoPrime,
    modularMultiplicativeInverse,
    extendedEuclid,
    getPrime1024
) where

import System.Random (RandomGen, randomRs, randomR)

-- | Computes a function f = mod (b^e) m efficiently.
modPow :: Integer -> Integer -> Integer -> Integer
modPow b e m = mPow (mod b m) e m 1 where
    mPow :: Integer -> Integer -> Integer -> Integer -> Integer
    mPow _ _ 1 _ = 0
    mPow _ 0 _ r = r
    mPow b e m r
        | odd e     = mPow (mod (b*b) m) (div e 2) m (mod (r*b) m)
        | otherwise = mPow (mod (b*b) m) (div e 2) m r

-- | Computes the totient for two prime numbers.
--   phi(n) = (p-1)*(q-1)
totient :: Integer -> Integer -> Integer
totient p q = (p-1)*(q-1)

-- | Returns a random tuple of two distinct prime numbers.
distinctPrimes :: RandomGen g => g -> ((Integer, Integer),g)
distinctPrimes g = let
    (fstPrime,g')  = getPrime1024 g
    (sndPrime,g'') = getPrime1024 g' in
        if fstPrime == sndPrime then distinctPrimes g'' else ((fstPrime,sndPrime),g'')

-- | Returns a random number that is co-prime to x.
getCoPrime :: RandomGen g => g -> Integer -> (Integer,g)
getCoPrime g x = (coPrimes !! i, g') where
        coPrimes = take 100000 [ y | y <- [2..x-1], isCoPrime x y]
        (i,g') = randomR (0,(length coPrimes)-1) g

-- | Checks if the integers x and y are co-prime.
isCoPrime :: Integer -> Integer -> Bool
isCoPrime x y
    | euclid x y == 1   = True
    | otherwise         = False

-- | Returns the greatest common divisor of x and y.
euclid :: Integer -> Integer -> Integer
euclid x y
    | y == 0    = x
    | otherwise = euclid y $ mod x y

-- | Returns modular multiplicative inverse for e modulo x using extendedEuclid.
modularMultiplicativeInverse :: Integer -> Integer -> Integer
modularMultiplicativeInverse e x = let (_, y, _) = extendedEuclid e x in y

-- | Computes modular multiplicative inverse for modulo x.
extendedEuclid :: Integer -> Integer -> (Integer, Integer, Integer)
extendedEuclid 0 x = (x,0,1)
extendedEuclid d x = let (g,s,t) = extendedEuclid (mod x d) d in
    (g, t-(div x d)*s, s)

-- TODO Replace 100 with a variable amount of tests.
-- | Returns a random 1024-Bit prime number.
getPrime1024 :: RandomGen g => g -> (Integer, g)
getPrime1024 = gP . get1024BitNumber where
    gP :: RandomGen g => (Integer, g) -> (Integer, g)
    gP (n, g')
        | even n        = gP (n+1,g')
        | mrt g' 100 n  = (n,g')
        | otherwise     = getPrime1024 g'
    get1024BitNumber :: RandomGen g => g -> (Integer, g)
    get1024BitNumber g = randomR (2^1023,(2^1024)-1) g

-- | Decides whether the given number is a prime number using the Miller-Rabin test.
--   k is the number of checks that will be performed.
mrt :: RandomGen g => g -> Int -> Integer -> Bool
mrt g k n
    | n == 2 || n == 3  = True
    | even n            = False
    | otherwise         = witness k n (d,j) randomBases where
        (d, j) = decompose (n-1) 0
        randomBases = bases g (n-2)

-- | Checks whether a number n is a prime number for some base values a.
--   k is the number of checks that will be performed.
--   (d,j) are numbers that describe n with (n-1) = 2^d * j.
--   (a:rands) is an infinite list of possible base values for mod a^d n.
witness :: Int -> Integer -> (Integer,Integer) -> [Integer] -> Bool
witness 0 _ _ _= True
witness k n (d,j) (a:rands)
    | modPow a d n == 1 || modPow a d n == n-1  = witness (k-1) n (d,j) rands
    | otherwise                                 = check (modPow a d n) (j-1) where
        check :: Integer -> Integer -> Bool
        check _ 0 = False
        check x count
            | modPow x 2 n == 1     = False
            | modPow x 2 n == n-1   = witness (k-1) n (d,j) rands
            | otherwise             = check (modPow x 2 n) (count-1)

-- | Returns an infinite list of values a where a is in range (2,upper).
bases :: RandomGen g => g -> Integer -> [Integer]
bases g upper = randomRs (2,upper) g

-- | Computes values d and j where n-1 = 2^d * j and n-1 = d in the initial call.
decompose :: Integer -> Integer -> (Integer,Integer)
decompose d j
    | odd d     = (d,j)
    | otherwise = decompose (div d 2) (j+1)

