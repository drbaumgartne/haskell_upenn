-- hw1.hs
-- Haskell UPenn Course
--
-- credit card validator

-- toDigits :: convert a 'long' integer into a list of its digits
-- use base 10 to our advantage
toDigits :: Integral a => a -> [a]
toDigits 0 = []
--toDigits n = n `mod` 10 : toDigits (n `div` 10) --more efficient for large numbers, but does digits reversed
toDigits n = toDigits (n `div` 10) ++ ([n `mod` 10]) -- 2nd part needs to be a list form

-- toDigitsRev :: reversed digits
-- can use reverse or define it...i'll define it
toDigitsRev 0 = []
toDigitsRev n = n `mod` 10 : toDigitsRev (n `div` 10) --more efficient for large numbers, but does digits reversed

-- doubleEveryOther :: double every other number beginning from the right
-- start with reversed list, pattern match the components
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:y:xs) = x : y*2 : doubleEveryOther xs

-- sumDigits :: sums a list of numbers
-- use basic sum, profit???
-- for 'practice' i'll define it
sumDigits :: Num a => [a] -> a
sumDigits = foldl (+) 0 -- basic fold

-- reDigit :: list of Ints redigitized
-- helper function to simply the validate command
reDigits :: [Integer] -> [Integer]
reDigits xs = concat (map toDigitsRev xs)

-- validate :: check if the double-every-other digitized credit card number, when divide by 10, equals 0
-- use all our functions
validate :: Integer -> Bool
validate n
    | ((sumDigits . reDigits . doubleEveryOther . toDigitsRev) n) `rem` 10 == 0 = True
    | otherwise                                                      = False


-- hw1.hs
-- Haskell UPenn Course
-- towers of hanoi solver

-- the rules for solving as given in the documentation
-- move n - 1 discs from a to c using b as temporary storage
-- move the top discs from a to b
-- move n - 1 discs from c to b using a as temporary storage

-- type synonyms to help
type Peg = String -- purely for clarity
type Move = (Peg, Peg) -- our action; will end up with a list of these

-- hanoi :: the final solver that takes number of discs and 3 pegs and returns a list of moves
-- can be solved recursively
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 source dest spare = [(source, dest)]
hanoi n source dest spare = hanoi (n-1) source spare dest ++ [(source, dest)] ++ hanoi (n-1) spare dest source

