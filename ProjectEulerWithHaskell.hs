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
--232792560

--Project Euler problem 6 : find the difference
-- between the sum of the squares of the first
--100 numbers

sumOfSquares n = sum $ map (\x -> x^2) [1..100]
squareOfSums n = (^2) . sum $ [1..100]

project_euler_6 = squareOfSums 100 - sumOfSquares 100
--25164150

--Project Euler problem 7 : find the 10001 prime number

primeList = filter isPrime [3,5..]

project_euler_7 = primeList !! 10000
--104759



--Project Euler problem 8 : fint the 13 adjacent
-- digits with the largest product

productList :: [Integer] -> [Integer]
productList foo
                | length foo < 13 = []
                | otherwise       = product (take 13 foo) : productList (tail foo)

toDigits :: Integer -> [Integer]
toDigits n
        | n < 1     = []
        | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

project_euler_8 = do
                str <- readFile "project_euler_8.txt"
                print . maximum . productList $ toDigits (read str :: Integer)
--23514624000


--Project Euler problem 9 : find the pythagorean
--triplet with a sum of 1000

pythagoreanTriplets :: Integer -> [[Integer]]
pythagoreanTriplets limit = [ [ a,b,c ] | m <- [1..floor . sqrt . fromIntegral $ limit]
                                        , n <- [1..(m-1)]
                                        , let a = m^2 - n^2
                                        , let b = 2*m*n
                                        , let c = m^2 + n^2
                                        , a + b + c == limit]

project_euler_9 = product . head . pythagoreanTriplets $ 1000
--31875000

--Project Euler problem 10 : sum of prime below 2000000

project_euler_10 = sum $ takeWhile (<2000000) primeList
--142913828920

isPrime' n = n `elem` primeList'
primeList' = 2 : filter isPrime' [3,5..]
