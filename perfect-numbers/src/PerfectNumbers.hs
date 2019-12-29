module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
    | n < 1 = Nothing
    | aliquotSum == n = Just Perfect
    | aliquotSum > n = Just Abundant
    | aliquotSum < n = Just Deficient
    | otherwise = Nothing
    where aliquotSum = sum [x | x <- [1..quot n 2], mod n x == 0]