# -*- coding: utf-8 -*-

import math
import random

# print(random)
# print(random.randrange(50, 101))-> 74

# Check if it's prime number or not
def is_prime(n):
    if n == 1: return False

    for k in range(2, int(math.sqrt(n)) + 1):
        if n % k == 0:
            return False

    return True

# Generate Prime Number Function
def primeGen ():
    r=random.randint(100000, 1000000000)
    if is_prime(r)==True:
        return r;
    else:
        return primeGen()
    
p=primeGen()
print (p)
q=primeGen()
# if p==q, generating q again
if p!=q:
    print (q)
else:
    print(primeGen())
