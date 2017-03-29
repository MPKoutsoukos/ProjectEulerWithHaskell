--Project Euler problem 1 : Sum of multiples of 3 and 5 under 1000

project_euler_1 = sum $ map (\x -> if x `mod` 3 ==0 || x `mod` 5 ==0 then x else 0) [1..999]
-- 233168

--Project Euler problem 2 : Sum of even valued fibonacci numbers under 4000000

fib = 0 : 1 : zipWith (+) fib (tail fib)

project_euler_2 = sum [x | x <- takeWhile(<= 4000000) fib, x `mod` 2 ==0]
