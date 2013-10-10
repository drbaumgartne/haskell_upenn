-- hw4.hs
-- Haskell UPenn Course
-- Higher-order

-- exercise 1 wholemeal programming
-- reimplementing some functions in a more idiomatic style
fun1 :: [Integer] -> Integer
fun1 []          = 1
fun1 (x:xs)
     | even x    = (x-2) * fun1 xs
     | otherwise = fun1 xs

-- my thoughts: we have a filter on even, some math and a recursive call
-- however, we really don't need the recursive call as all it will do is check its even and add it to our
-- multiplication sequence

fun1' :: [Integer] -> Integer
fun1' [] = 1
fun1' nums = foldr (*) 1 $ map (subtract 2) $ filter even nums

-- part 2
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n    = n + fun2 (n `div` 2) 
    | otherwise = fun2 (3 * n + 1)

-- there's a way to do this on one line?!? without using if then else? or a case expr?
-- ok...this predominately a sum of a list of numbers, if there even we take that number add it it our list
-- then divide it by t
-- if it was odd, we triple the number and add 1 (which will get us to an even)
