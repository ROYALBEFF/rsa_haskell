module RSA(
    PublicKey,
    PrivateKey,
    encrypt,
    decrypt,
    generateKeyPair
) where

import System.Random (mkStdGen)
import CryptoMath (modPow, distinctPrimes, totient, getCoPrime, modularMultiplicativeInverse)

-- | First integer is the modulus
--   second integer is the public key exponent
type PublicKey = (Integer, Integer)

-- | First integer is the modulus
--   second integer is the private key exponent.
type PrivateKey = (Integer, Integer)

-- | RSA encryption primitive.
--   Encrypts message m with public key (n,e) where
--   n is the modulus and e is the public key exponent.
encrypt :: Integer -> PublicKey -> Integer
encrypt m (n,e) = modPow m e n

-- | RSA decryption primitive.
--   Decrypts ciphertext c with private key (n,d) where
--   n is the modulus and d is the private key exponent.
decrypt :: Integer -> PrivateKey -> Integer
decrypt c (n,d) = modPow c d n

-- | Generates a new private key the following way
--   1) Choose two distinct prime numbers p and q.
--   2) Compute n = p*q.
--   3) Compute phi(n) = (p-1)*(q-1).
--   4) Choose integer d so that d and phi(n) are co-prime.
--   5) Determine e as the multiplicative inverse of d (mod phi(n)).
generateKeyPair :: Int -> (PrivateKey,PublicKey)
generateKeyPair seed = let
    g = mkStdGen seed
    ((p, q),g') = distinctPrimes g
    n = p*q
    phi = totient p q
    (e,_) = getCoPrime g' phi
    d = modularMultiplicativeInverse e phi in
        if d > phi then ((n,mod d phi),(n,e))
        else if d < 0 then ((n,d+phi),(n,e))
        else ((n,d),(n,e))
