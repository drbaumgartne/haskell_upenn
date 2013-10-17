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
fun1' nums = product $ map (subtract 2) $ filter even nums

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


{- exercise 2 fold tree -}
-- implement a binary three using foldr
-- also must be balanced (all trees and subtrees are no more than 1 apart), order not important
-- if order was important then we would be talking about creating a complex data structure probably outside
-- the scope of the 4th homework
-- data type for the tree
data Tree a = Leaf
            | Node Int a (Tree a) (Tree a)
            deriving (Show, Eq)
-- count the tree height as part of the insert process
-- way to insert a single item
-- way to insert a list of items while updating the height calculation
foldTree :: [a] -> Tree a
foldTree = foldr treeInsert Leaf

-- singleTree :: inserts an item into a single tree
singleTree :: a -> Tree a
singleTree x = Node 0 x Leaf Leaf

--treeInsert :: create a new tree and taking the old and new and combining them in some fashion
treeInsert :: a -> Tree a -> Tree a
treeInsert a Leaf = singleTree a -- 1
treeInsert a (Node i ao Leaf Leaf) = Node (i+1) ao (treeInsert a Leaf) Leaf -- 2
treeInsert a (Node i ao left Leaf) = Node i ao left (treeInsert a Leaf) -- 3
treeInsert a (Node i ao left@(Node il al ll rl) right@(Node ir ar lr rr))
    | il == ir && sTC left == sTC right && odd (sTC left) = Node (i+1) ao (treeInsert a left) right -- 4
    | il > ir  = Node i ao left (treeInsert a right) -- 5
    | il == ir && sTC left == sTC right && even (sTC left) = Node i ao (treeInsert a left) right -- 6
    | il == ir && sTC left /= sTC right = Node i ao left (treeInsert a right) -- 7
-- still not quite perfect with the height of the parent values

--valueTreeList :: build a list of child tree values
valueTreeList :: Tree a -> [Int]
valueTreeList Leaf = []
valueTreeList (Node i a left right) = [i] ++ valueTreeList left ++ valueTreeList right

--sumTreeValues :: sums the values of all the child trees
sumTree = sum . valueTreeList

--countTreeList :: builds a list of childs, counting each as 1
countTreeList :: Tree a -> [Int]
countTreeList Leaf = [0]
countTreeList (Node i a left right) = [1] ++ countTreeList left ++ countTreeList right

--sumTreeCount (sTC) :: sums the counts of all the child trees
sTC = sum . countTreeList

{- exercise 3 more folds -}
-- implement xor, true whenever an odd number of inputs is true
xor :: [Bool] -> Bool
xor = foldr1 (\x acc -> (x || acc) && not (x && acc))

-- implement map with foldr
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []
