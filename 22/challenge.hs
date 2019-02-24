data Triplet a = Triplet a a a deriving Show

mapTriplet :: (a -> b) -> Triplet a -> Triplet b
mapTriplet f (Triplet a b c) = Triplet (f a) (f b) (f c)

data Tuple a b = Tuple a b deriving Show

fstT :: Tuple a b -> a
fstT (Tuple x _) = x

sndT :: Tuple a b -> b
sndT (Tuple _ y) = y

toTuple :: (a, b) -> Tuple a b
toTuple (x, y) = Tuple x y

fromTuple :: Tuple a b -> (a, b)
fromTuple (Tuple x y) = (x, y)

data MyMaybe a = MyJust a | MyNothing deriving Show

myFromMaybe :: a -> MyMaybe a -> a
myFromMaybe _ (MyJust x) = x
myFromMaybe d MyNothing = d

myMaybe :: b -> (a -> b) -> MyMaybe a -> b
myMaybe _ f (MyJust x) = f x
myMaybe d _ MyNothing = d

data Matryoshka = Nest Matryoshka | Term deriving Show
countM :: Matryoshka -> Integer
countM (Nest m) = 1 + (countM m)
countM Term = 0

makeM :: Integer -> Matryoshka
makeM x  
  | x > 0 = Nest (makeM $ x-1)
  | x == 0 = Term

data Tree a = Atom a | Branch (Tree a) (Tree a) | TriBranch (Tree a) (Tree a) (Tree a) deriving Show
dfs :: Tree a -> [a]
dfs (Atom x) = [x]
dfs (Branch l r) = dfs l ++ dfs r
dfs (TriBranch l c r) = dfs l ++ dfs c ++ dfs r
