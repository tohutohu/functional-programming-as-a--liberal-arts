data Human = Human { name :: String, age :: Integer } deriving Show

info :: Human -> String
info Human { name = name, age = age } = name ++ " (" ++ show age ++ ")"
