module SecretHandshake (handshake) where

handshake :: Int -> [String]
handshake n = if reverseFlag binary
    then reverse toEvent binary
    else toEvent binary
    where 
        binary = toBin n
        reverseFlag bin = 

toBin :: Int -> [Int]
toBin 0 = [0]
toBin 1 = [1]
toBin n = toBin (quot n 2) ++ [rem n 2]

toEvent :: [Int] -> [String]
toEvent = error "lmao"