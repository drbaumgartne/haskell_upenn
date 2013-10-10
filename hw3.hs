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

--numIter :: ([a] -> [a]) -> b -> [a] -> [a]
numIter f 0 _ = []
numIter f n list = f n list : numIter f (n-1) list
