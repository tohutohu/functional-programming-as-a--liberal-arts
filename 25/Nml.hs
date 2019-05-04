module Nml (Nml, nml, fromNml) where

import Data.Char (isSpace)
import Data.Tree (Tree(..))
import Data.List (unfoldr)

type Nml = Tree String

data Token = Open String | Close String | Text String deriving Show

token :: String -> Maybe (Token, String)
token "" = Nothing
token ('<' : '/' : s) = tag Close s
token ('<' : s) = tag Open s
token s | all isSpace tx = token r
        | otherwise = Just (Text tx, r)
        where (tx, r) = span (/= '<') s

tag :: (String -> Token) -> String -> Maybe (Token, String)
tag f s = case span (/= '>') s of
            (tg, _ : r) -> Just (f tg, r)
            _ -> Nothing

parse :: [Token] -> Maybe (Nml, [Token])
parse (Open o : ts) = case parseL ts of
                        (ns, Close c : r) | o == c -> Just (Node o ns, r)
                        _ -> Nothing
parse (Text tx : ts) = Just (Node tx [], ts)
parse _ = Nothing

parseL :: [Token] -> ([Nml], [Token])
parseL ts = case parse ts of
              Nothing -> ([], ts)
              Just (n, r) -> (n : ns, r')
                where (ns, r') = parseL r

nml :: String -> Maybe Nml
nml = fmap fst . parse . unfoldr token

fromNml :: Nml -> String
fromNml = toString 0 . toTokens

toTokens :: Nml -> [Token]
toTokens (Node tx []) = [Text tx]
toTokens (Node tg ns) = Open tg : concatMap toTokens ns ++ [Close tg]

toString :: Int -> [Token] -> String
toString i (Open o : Text tx : Close c : ts) =
  idt i (opn o ++ tx ++ cls c) ++ toString (i + 1) ts
toString i (Open o : ts) = idt i (opn o) ++ toString (i + 1) ts
toString i (Close c : ts) = idt (i - 1) (cls c) ++ toString (i - 1) ts
toString i (Text tx : ts) = idt i tx ++ toString i ts
toString _ [] = ""

sample1 :: Nml
sample1 = Node "hello" [Node "world" []]

idt :: Int -> String -> String
idt i = (replicate i '\t' ++) . (++ "\n")

opn, cls :: String -> String
opn = ('<' :) . (++ ">")
cls = ("</" ++) . (++ ">")
