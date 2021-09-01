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

p=223 --prime order for DLP
g=13  --generator    gcd(p,g)==1
m=100  --massage
priKey=17  --private key
pubKey=modExp g priKey p  --public key


signature :: Integer -> Integer -> (Integer,Integer)
signature r m = let
     x = modExp g r p
     e = x + m -- hash(x||m)
     y = (12 - (priKey*e `mod` p)) `fullMod` p
     in (y,e)


verify :: Integer -> Integer -> Integer -> Bool
verify y e m = if ev == e then True else False where
  ev = (rv + m)  where
     rv = (modExp g y p) * (modExp pubKey e p) `fullMod` p

--できない
verify2 :: Integer -> Integer -> Integer -> Integer
verify2 y e m =  rv  where
     rv = ((modExp g y p) * (modExp pubKey e p)) `fullMod` p
