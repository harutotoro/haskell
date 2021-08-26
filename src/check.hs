f :: Maybe Int
f = do
  x <- return 1
  let y = x + 1
      z = 2
  return (y+z)

g :: Maybe Int
g = do
  x <- return 1
  return ((x+1)+z)
  where
    z = 2
