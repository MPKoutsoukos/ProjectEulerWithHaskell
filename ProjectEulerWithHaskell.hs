--Project Euler problem 1 : Sum of multiples of 3 and 5 under 1000

project_euler_1 = sum $ map (\x -> if x `mod` 3 ==0 || x `mod` 5 ==0 then x else 0) [1..999]
-- 233168
