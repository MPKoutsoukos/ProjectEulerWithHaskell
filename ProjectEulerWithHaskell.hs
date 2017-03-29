--Project Euler problem 1 : Sum of
--multiples of 3 and 5 under 1000

project_euler_1 = sum $ map (\x -> if x `mod` 3 ==0 || x `mod` 5 ==0 then x else 0) [1..999]
-- 233168

--Project Euler problem 2 : Sum of even
--valued fibonacci numbers under 4000000

fib = 0 : 1 : zipWith (+) fib (tail fib)

project_euler_2 = sum [x | x <- takeWhile(<= 4000000) fib, x `mod` 2 ==0]
-- 4613732


--Project Euler problem 3 : largest prime factor of 600851475143

isPrime :: Integer -> Bool
isPrime n
        | n <= 1 = False
        | n == 2  = True
        | n `mod` 2 ==0 = False
        | otherwise = length [ x | x <- [3..ceiling . sqrt . fromIntegral $ n], n `mod` x ==0] ==0

listOfPrimeDiv :: Integer -> [Integer]
listOfPrimeDiv n = [ x | x <- [2..ceiling . sqrt . fromIntegral $ n], n `mod` x ==0 && isPrime x]

project_euler_3 = last . listOfPrimeDiv $ 600851475143
--6857


--Project EUler problem 4 : the largest palidrome product
-- of two three digit numbers

isPalidrome :: Integer -> Bool
isPalidrome n = reverse (show n) == show n

project_euler_4 = maximum [ x | a <- [100..999], b <- [a..999], let x = a*b, isPalidrome x]
--906690

--Project Euler problem 5 : smallest number to be evenly
--divisible by all numbers from 1 to 20

project_euler_5 = foldr1 lcm [1..20]
