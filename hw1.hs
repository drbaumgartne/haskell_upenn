-- hw1.hs
-- UPenn Online Haskell Course
--

doubleSecond :: [Integer] -> [Integer]
doubleSecond [] = []
doubleSecond (x:y:xs) = x*2 : y : doubleSecond xs

--doubleSecond' :: [Integer] -> [Integer]
--doubleSecond' (x:xs)
--    | isEven xs =

isEven :: [Integer] -> Bool
isEven xs
    | length xs `rem` 2 == 0 = True
    | otherwise              = False

isOdd xs
    | isEven xs = False
    | otherwise = True

toDigits :: Integral a => a -> [a]
toDigits 0 = []
toDigits x = x `mod` 10 : toDigits (x `div` 10)

properDigits xs = reverse $ toDigits xs
listDigits xs = concat $ map toDigits xs

validate x
    | (sum $ listDigits $ doubleSecond $ properDigits x) `mod` 10 == 0 = True
    | otherwise = False
