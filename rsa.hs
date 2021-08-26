import System.Random
import Data.Bits

--random generator  between A and B
randomInteger :: Integer -> Integer -> IO Integer
randomInteger m n = randomRIO (m, n)

--Extended Euclidean Algorithm
xgcd :: Integer -> Integer -> (Integer, Integer, Integer)
xgcd x y
  | x < 0 = let (a, b, c) = xgcd (-x) y in (-a, b, c)
  | y < 0 = let (a, b, c) = xgcd x (-y) in (a, -b, c)
  | x < y = let (a, b, c) = xgcd y x in (b, a, c)
  | y == 0 = (1, 0, x)
  | otherwise = let
    (q, r) = x `divMod` y
    (a', b', c') = xgcd y r
  in (b', a' - q*b', c')

--Inverse Element
inverse :: Integer -> Integer -> Integer
inverse x y = a `mod` y where
  (a, b, c) = xgcd x y

--GCD (The Greatest Common Divisor)
gcd' :: (Integer, Integer) -> Integer
gcd' (a, b) =
  if b == 0 then a else gcd' (b, a `mod` b)

--LCM (The Least Common Multiple)
lcm' :: (Integer, Integer) -> Integer
lcm' (a, b) = a * b `div` gcd' (a, b)

--Prime Judgement
isPrime :: Integer -> Bool
isPrime 2 = True
isPrime n
    | n < 2 = False
    | n `mod` 2 == 0 = False
    | otherwise = try n 3
    where
        try :: Integer -> Integer -> Bool
        try m i
            | m `mod` i == 0 = False
            | i^2 > m = True
            | otherwise =  try m (i+2)

--Prime Number Generator
primegen :: (Integer,Integer) ->  IO Integer
primegen (m,n) = do
               x <- randomInteger m n
               if isPrime x == True
                  then return x
                  else primegen (m,n)
--IO Integer とInteger の互換性は死にたい

--Find E from sig   
{- getE :: Integer -> IO Integer
getE v = do
        e <- randomInteger 2 (v - 1)
        if gcd e v == 1
            then return e
            else getE v-}
--IO Integer とInteger の互換性は死にたい

--Key Generator
keygen :: (Integer, Integer) -> (Integer, Integer, Integer, Integer) --input(p,q)-> output(n,sig,e,d)
keygen (p,q) = let
    n = p*q
    sig = (p-1)*(q-1)
    --e = getE sig
    e=5
    d = inverse e sig
    in (n, sig, e,d)

--RSA Encrypto
encrypt :: Integer -> (Integer,Integer) -> Integer
encrypt m (e,n) = modExp m e n     -- c= m^e mod n

--RSA Decrypto
decrypt :: Integer -> (Integer,Integer) -> Integer
decrypt c (d,n) = modExp c d n     -- m= c^d mod n

--Binary Method (Modular Exponentiation)
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m where t = if testBit e 0 then b `mod` m else 1


