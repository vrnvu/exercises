module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard p xs = foldr (\h t -> if p h then t else h:t) [] xs 

keep :: (a -> Bool) -> [a] -> [a]
keep p xs = [x | x <- xs, p x]
