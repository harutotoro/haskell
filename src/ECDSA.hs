--import Data.Bits
import Data.Digest.Pure.SHA
--import qualified Data.ByteString as B

a=0;  -- parameters Y^2=X^3+aX+b and 4a^3+27b^2!=0
b=7;  --   Y^2=X^3+7 
p=223 -- Finite Field Parameter
--G=(15,18)   --  Base point
n=17
h=2
----------------------------------------------------------------
-- Generate key
-- chose random d between [1,n-1] --d=13
-- cluc Q=dG
-- Q is pubKey
----------------------------------------------------------------
-- Generate signature 
-- chose random k between [1,n-1]
-- cluc kG (x1,x2)and r=x1 mod n 

-- e=sha256(m) hash function 
-- s= k^(-1)(e+dr) mod n -> sig(r,s)
----------------------------------------------------------------
-- Verify 
-- check r,s 