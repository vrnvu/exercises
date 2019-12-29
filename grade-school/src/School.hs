module School (School, add, empty, grade, sorted) where

type Grade = (Int, [String])
type School = [Grade]

add :: Int -> String -> School -> School
add gradeNum student [] = [(gradeNum, [student])]
add gradeNum student (x:xs) = if fst x == gradeNum
    then (gradeNum, snd x ++ [student]) : xs
    else x : add gradeNum student xs

empty :: School
empty = []

grade :: Int -> School -> [String]
grade _ [] = []
grade gradeNum (x:xs) = if (fst x) == gradeNum 
    then snd x 
    else grade gradeNum xs

sorted :: School -> School
sorted = sortGrades . sortNames

sortNames :: School -> School
sortNames = fmap sortNamesInGrade 

sortNamesInGrade :: Grade -> Grade
sortNamesInGrade g = (fst g, quicksortNames (snd g)) 

quicksortNames :: [String] -> [String]
quicksortNames [] = []
quicksortNames (x:xs) = (quicksortNames lesser) ++ [x] ++ (quicksortNames greater)
    where
        lesser = filter (< x) xs
        greater = filter (> x) xs

sortGrades :: School -> School
sortGrades [] = []
sortGrades (x:xs) = (sortGrades lesser) ++ [x] ++ (sortGrades greater)
    where
        lesser = filter (\g -> fst x > fst g) xs
        greater = filter (\g -> fst x < fst g) xs