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
-- Pedersen Commitment

--choose prime q then p<-2q+1,r<-2 such as q=11 then p=23,r=2

p=29 --prime number for Schnorr group
q=7  --prime oerder for Schnorr group
r=4  -- p=qr+1
h=13 -- chose between 1<h<p
g=25  --generator   g=h^r (mod p) &&gcd(p,g)==1, g!=1
m=8  --massage
s=5  --secret number
d=3  --random number
f=modExp g s p

--https://asecuritysite.com/encryption/ped

commitment :: Integer -> Integer -> Integer
commitment d m = c where
     c = ((modExp g m p)*(modExp f d p)) `fullMod` p

verify :: Integer -> Integer -> Integer -> Bool
verify c r m = if cv == c then True else False where
  cv = ((modExp g m p) * (modExp f d p)) `fullMod` p  
