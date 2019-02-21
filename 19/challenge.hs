concatMapF :: (a -> [b]) -> [a] -> [b]

concatMapF f a = foldr (++) [] (map f a)

concatMapRaw :: (a -> [b]) -> [a] -> [b]
concatMapRaw f (a:as) = (++) (f a) (concatMapRaw f as)
concatMapRaw _ _ = []

% Prelude> [(x, y, z) | x <- [1..5], y <- [1..5], z <- [1..5], x <= y + z]
% Prelude> [(x, y, z) | x <- [1..5], y <- [1..5], z <- [1..5], x <= y + z, x <= y, y <= z]
