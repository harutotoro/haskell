import Data.Bits

inverse :: Integer -> Integer -> Integer
inverse x y = a `mod` y where
  (a, b, c) = xgcd x y

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

fullMod :: Integer -> Integer -> Integer
fullMod a b
    | a >= 0 = a `mod` b
    | otherwise = fullMod (a + b) b

-- Prime Judgement
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

-- GCD (The Greatest Common Divisor)
gcd' :: (Integer, Integer) -> Integer
gcd' (a, b) =
  if b == 0 then a else gcd' (b, a `mod` b)

  -- Binary Method (Modular Exponentiation)

modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m where t = if testBit e 0 then b `mod` m else 1
---------------------------------------------------------
-- Schnorr Signature

p=29 --prime number for Schnorr group
q=7  --prime oerder for Schnorr group
r=4  -- p=qr+1
h=13 -- chose between 1<h<p
g=25  --generator   g=h^r (mod p) &&gcd(p,g)==1, g!=1
m=8  --massage
priKey=5  --private key 
pubKey=modExp g priKey p  --public key

--https://en.wikipedia.org/wiki/Schnorr_signature
--priKey,random k <=Schnorr group (mod q) 1<_<q
--s,e,ev <= Schnorr group (mod q)
--pubKey,r,rv <= group (mod p)

signature :: Integer -> Integer -> (Integer,Integer)
signature k m = let
     r = modExp g k p    --g^k (mod) p
     e = (r + m) `mod` q -- hash(r||m) mod q
     s = (k - (priKey*e `mod` q)) `fullMod` q 
     in (s,e)

verify :: Integer -> Integer -> Integer -> Bool
verify s e m = if ev == e then True else False where
  ev = (rv + m) `mod` q where  -- hash(rv||m) mod q rv==r
     rv = ((modExp g s p) * (modExp pubKey e p)) `fullMod` p  --g^s*pubKey^e (mod) p
