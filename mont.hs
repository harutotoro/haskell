import Prelude
import Test.QuickCheck

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

propXgcd :: Integer -> Integer -> Bool
propXgcd x y = a*x + b*y == c where
  (a, b, c) = xgcd x y

inverse :: Integer -> Integer -> Integer
inverse x y = a `mod` y where
  (a, b, c) = xgcd x y

propInverse :: Integer -> Integer -> Bool
propInverse 0 _ = True
propInverse _ 0 = True
propInverse 1 _ = True
propInverse _ 1 = True
propInverse x y = if y < 0 then True else if gcd x y == 1 then ((inverse x y)*x `mod` y) == 1 else True where
  gcd x y = c where
    (a, b, c) = xgcd x y

r = 10
n = 997
r_2=100
r_3=1000
m = 3
r_inverse = inverse r n
minus_n_inverse = inverse (-n) r

toMontgomery :: Integer -> Integer
toMontgomery x = x*r `mod` n

toMontgomery2 :: Integer -> Integer
toMontgomery2 x = x*r_3 `mod` n

fromMontgomery :: Integer -> Integer
fromMontgomery y = y*r_inverse `mod` n

fromMontgomery2 :: Integer -> Integer
fromMontgomery2 y = y*r_inverse*r_inverse*r_inverse `mod` n

propMontgomeryIso1 :: Integer -> Bool
propMontgomeryIso1 x = fromMontgomery (toMontgomery (x `mod` n)) == x `mod` n

propMontgomeryIso2 :: Integer -> Integer -> Bool
propMontgomeryIso2 x y = (toMontgomery x + toMontgomery y) `mod` n == toMontgomery ((x + y) `mod` n)

propMontgomeryIso3 :: Integer -> Integer -> Bool
propMontgomeryIso3 x y = (toMontgomery x * toMontgomery y) `mod` n == toMontgomery ((x*y) `mod` n) -- should fail

montMult :: Integer -> Integer -> Integer
montMult 0 _ = 0
montMult _ 0 = 0
montMult a b = if s < n then s else s - n where
  t = a*b
  x = (t `mod` r)*minus_n_inverse `mod` r
  s = (t + x*n) `div` r

propMontgomeryIso4 :: Integer -> Integer -> Bool
propMontgomeryIso4 x y = (toMontgomery x `montMult` toMontgomery y) `mod` n == toMontgomery ((x*y) `mod` n) -- should work

montRedu :: Integer -> Integer
montRedu 0  = 0
montRedu t = if s < n then s else s - n where
  x = (t `mod` r)*minus_n_inverse `mod` r
  s = (t + x*n) `div` r

propMontgomeryIso5 :: Integer -> Integer -> Bool
propMontgomeryIso5 x y = (x `montMult` y) `mod` n == (montRedu (x*y) )`mod` n -- should work

loop ::  Integer -> Integer -> Integer -> Integer ->Integer
loop a b con s= if con == m then s else loop a b (con+1) (montRedu(s+(a* ((b `mod` r^(con+1)) `div`(r^con)))))

montMult2 :: Integer -> Integer -> Integer
montMult2 0 _ = 0
montMult2 _ 0 = 0
montMult2 a b = if s < n then s else s - n where
   s= loop a b 0 0

propMontgomeryIso6 :: Integer -> Integer -> Bool
propMontgomeryIso6 x y = fromMontgomery((toMontgomery(x) `montMult` toMontgomery(y)) `mod` n) == fromMontgomery2((toMontgomery2(x) `montMult2` toMontgomery2(y))) `mod` n -- should work
