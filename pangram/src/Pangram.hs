module Pangram (isPangram) where

import qualified Data.Set as Set
import Data.Char

type Dictionary = Set.Set Char

isPangram :: String -> Bool
isPangram text = reduce alphabet $ fmap toLower text
    where 
    reduce set "" = length set == 0
    reduce set (x:xs) = reduce (Set.delete x set) xs

isPangram2 :: String -> Bool
isPangram2 = reduce alphabet
    where 
    reduce set "" = length set == 0
    reduce set (x:xs) = reduce (remove x set) xs
   
alphabet :: Dictionary
alphabet = Set.fromList "abcdefghijklmnopqrstuvwxyz"

remove :: Char -> Dictionary -> Dictionary
remove c = Set.delete c . Set.delete (toLower c) 