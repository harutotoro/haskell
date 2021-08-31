--import Data.Bits
--import Data.Digest.Pure.SHA
--import qualified Data.ByteString as B

a=0;  -- parameters Y^2=X^3+aX+b and 4a^3+27b^2!=0
b=7;  --   Y^2=X^3+7 (mod p)
p=223 -- Finite Field Parameter
gx=47   --  Base point (gx,gy)
gy=71
n=31
h=2

fullMod :: Integer -> Integer -> Integer
fullMod a b
    | a >= 0 = a `mod` b
    | otherwise = fullMod (a + b) b

-- Calculating r^(-1)  inverse
inverse :: Integer -> Integer -> Integer
inverse x y = a `fullMod` y where
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

-- Calculate Elliptic Curve
addElli :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
addElli (x1,y1) (x2,y2) = let 
    r = if x1 == x2 && y1 == y2 then ((3*x1^2 + a)`fullMod` p)*inverse (2*y1) p `fullMod` p else (y2-y1)*inverse (x2-x1) p `fullMod` p
    x3 = (r^2 - (x1+x2)) `fullMod` p
    y3 = (r*(x1-x3)-y1) `fullMod` p
    in (x3,y3)

check :: Integer -> Bool
check z = if z>=1 && z <= (n-1) then True else False 


multElli :: (Integer,Integer) -> Integer -> (Integer,Integer)
multElli (x1,y1) j = 


--modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m where t = if testBit e 0 then b `mod` m else 1
----------------------------------------------------------------
-- Generate key

d=5   -- chose random d between [1,n-1] 
-- cluc Q=dG (126,96), Q is pubKey
----------------------------------------------------------------
-- Generate signature 

k= 8    -- chose random k between [1,n-1]
-- cluc kG (x1,x2)and 
kgx1=116
rk=kgx1 `mod` n    --r=x1 mod n (116,55)
e=100000  -- e=sha256(m) hash function 
s = inverse k n * (e+d*rk) `mod` n     -- s= k^(-1)(e+dr) mod n -> sig(r,s)
----------------------------------------------------------------
-- Verify signature

-- check r,s  between [a,n-1]
-- e=sha256(m) hash function 
w = inverse s n      -- w= s^(-1) mod n
u1 = (e*w) `mod` n    --28  u1=ew(modn)
u2 = (rk*w) `mod` n    --27 u2=rw(modn)
-- cluc u1G + u2G->(x2,y2) (116,168)
xx=116
v= xx `mod` n  -- v=x2(modn)
-- check rk==v