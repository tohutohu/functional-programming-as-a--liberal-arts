import Data.Char
import Data.Maybe

data War = Kinoko | Takenoko

habatsu :: (War, String) -> String
habatsu (Kinoko, name) = name ++ " (Kinoko)"
habatsu (Takenoko, name) = name ++ " (Takenoko)"

data Day = Monday | Tuesday | Wednesday | Thursday | Fryday | Satuday | Sanday deriving Show

plan :: Day -> String
plan Sanday = "play"
plan _ = "work"

data Building = Building String Integer deriving Show

height :: Building -> String
height (Building name h) = name ++ " :" ++ show h ++ "m"

data Point = Cartesian Double Double | Polar Double Double deriving Show
data Shape = Circle Point Double | Square Point Double | Rectangle Point Double Double deriving Show

square :: Shape -> Shape
square (Rectangle point width height) | width == height = Square point width
square shape = shape

toJustUpper = Just . toUpper

data CharNum = Char :*: Int deriving Show
enc :: [CharNum] -> String
enc = concat . map (\(c :*: n) -> replicate n c)

bin = ['f':*:1, 'o':*:2]

