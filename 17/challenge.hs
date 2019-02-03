mul, tmul :: [Integer]

twoTime :: Integer -> Integer
twoTime n = 2 * n
mul@(_ : tmul) = 0: 1: zipWith (+) tmul (map twoTime mul)
