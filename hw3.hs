-- hw3.hs
-- Haskell UPenn Course
-- Code Golf

-- exercise 1 hopscotch
-- must be of the form [a] -> [[a]]
-- first list must be the input list, 2nd must contain every second element from the input list, nth list should contain every nth element from the list
-- no use of head, tail, init, !!, etc, but any other standard functions allowed
-- first thought a (n+1) recursive function that uses a helper of drop and take
-- something with a fold or map (length)

-- first attempt
skips :: [a] -> [[a]]
skips list = reverse $ numIter dropTake (length list) list

dropTake :: Int -> [a] -> [a]
dropTake n [] = []
dropTake n list = ((take 1) . (drop (n-1))) list ++ ((dropTake n) . (drop n)) list

--numIter :: (Int -> [a] -> [a]) -> Int -> [a] -> [a]
numIter f 0 _ = []
numIter f n list = f n list : numIter f (n-1) list


-- exercise 2 local maximum
-- must be of the form [Integer] -> [Integer]
-- finds all local maximum's in a list of numbers
-- local max == a,b,c  b > a && b > c
-- first thought, a complex list pattern match with 4 labeled factors

-- first attempt
localMaximum :: [Integer] -> [Integer]
localMaximum [] = []
localMaximum (x:[]) = []
localMaximum (x:y:[]) = []
localMaximum (x:y:zs) = case (x < y && y > (head zs)) of
    True -> y : localMaximum (y:zs)
    False -> localMaximum (y:zs)


-- exercise 3 histogram
-- must be of the form [Integer] -> String
-- will always take a list of integers from 0 to 9 and should output a vertical histogram
-- first thoughts, going to need lots of helper functions to check which numbers we have, sort 'em, count 'em
-- and generate the lines of text to match what a vertical histogram looks like
-- plus some general one that spits out the bottom detail

-- first attempt
histogram :: [Int] -> String
histogram nums = (concat rows) ++ footer
    where rows = numIter buildRow (height histList) histList
          histList = reverse $ numIter countNum 10 nums

footer :: String
footer = "==========\n0123456789\n"

-- thought, filter (==n) [list] will give us a list of each number ==n, the length of that would be our vertical bar
-- if we iterate that n times (from our decreasing numIter function above) we should get a count of each of the numbers we are looking for
countNum :: Int -> [Int] -> Int
countNum n list = length $ filter (==(n-1)) list -- (n-1) so we are doing 9 to 0, not 10 to 1
-- to be used as... numIter countNum 10 list


-- total height of the histogram is just the max of the counted numbers
height :: [Int] -> Int
height l = maximum l

-- need something that checks if each number is >= height, if so give a "*" else give " "
-- sounds like a map or a fold with a filter, maybe a list comprehension?
-- again, using numIter I should be able to do buildRow a bunch of times
buildRow :: Int -> [Int] -> String
buildRow n list = (concat $ map (convert . (>=n)) list) ++ "\n"

convert :: Bool -> String
convert x
    | x == True  = "*"
    | x == False = " "
