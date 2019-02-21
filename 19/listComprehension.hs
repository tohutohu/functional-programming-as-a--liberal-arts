import Data.Bool (bool)

filterC :: (a -> Bool) -> [a] -> [a]
filterC p = concatMap $ \x -> bool [] [x] (p x)

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = concat . map f


