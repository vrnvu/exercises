module RunLength (decode, encode) where

import Data.List
import Data.Char(intToDigit)

import Text.Parsec.Integers (Parser)
import FunctionsAndTypesForParsing (regularParse)
import Text.Parsec.Combinator (many1)

type Tuple = (Int, Char)
type Stack = [Tuple]

-- parse integers, parse chars, zip = Tuple(int, char)
decode :: String -> String
decode encodedText = reduce [] (toTuple encodedText)
    where
        reduce stack [] = stack
        reduce stack (x:xs) = reduce (pop stack x) xs

toTuple :: String -> [Tuple]
toTuple _ = []

pop :: [String] -> Tuple -> [String]
pop stack tuple = if counter == 0
    then stack ++ [[c]]
    else pop (stack ++ [[c]]) (counter - 1, c)
    where 
        counter = count tuple
        c = char tuple 

encode :: String -> String
encode text = reduce [] text
    where
        reduce stack "" = format stack
        reduce stack (x:xs) = reduce (push stack x) xs

push :: Stack -> Char -> Stack
push stack c = if char current == c
    then (init stack) ++ [(count current + 1, c)]
    else stack ++ [(0, c)]
    where current = peek stack

peek :: Stack -> Tuple
peek = last

format :: Stack -> String
format stack = intercalate "" $ fmap toString stack

count :: Tuple -> Int
count = fst

char :: Tuple -> Char
char = snd

toString :: Tuple -> String
toString (0, c) = [c]
toString (n, c) = intToDigit n : c : []