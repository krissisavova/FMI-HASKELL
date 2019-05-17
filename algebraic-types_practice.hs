-- Algebraic types (Practice file)

main :: IO()
main = let tree = (Node 3 (Node 1 (Node 0 Empty Empty) (Node 2 Empty Empty)) (Node 4 Empty (Node 5 Empty (Node 6 Empty Empty))))
in do

-- 2
	print $ diet Apple
	print $ diet Banitsa
	
-- EX. 1
	print (mkList [1, 2, 3])
	print (listEmpty (1 `Cons` (2 `Cons` (3 `Cons` Nil))))
	print $ listHead (1 `Cons` (2 `Cons` (3 `Cons` Nil)))
	print $ listTail (1 `Cons` (2 `Cons` (3 `Cons` Nil)))
	print $ listMap (+1) (1 `Cons` (2 `Cons` (3 `Cons` Nil)))
    print $ listFilter even (1 `Cons` (2 `Cons` (3 `Cons` Nil)))
	
-- EX. 2
	print $ findTreeDepth tree
	print $ findTreeDepth Empty
	print $ findTreeElem 5 tree  
	print $ findTreeElem 5 Empty
	print $ findTreeNodes Empty 
	print $ findTreeNodes tree
	print $ sumTreeValues tree
	print $ sumTreeValues Empty
	
	
--1
-- write a function "weather" which determines what the temperature could usually be for different types of weather 
type Weather = Snowy | Windy | Cloudy | Rainy | Stormy | Foggy | Moderate | Sunny
type Temperature = Cold | Medium | Hot 

weather :: Weather -> Temperature
weather Sunny = Hot 
weather Moderate = Medium
weather _ = Cold 

--2
type Food = Apple | PotatoChips | Hummus | Banitsa | Pizza | IceCream | Banana | Chocolate
type Category = Healthy | Unhealthy

diet :: Food -> Category
diet Apple = Healthy
diet Hummus = Healthy
diet Banana = Healthy
diet _ = Unhealthy

--3 -> small practices
 -- define a function of type Tree describing a random binary tree 
data BTree a = Empty | Node a (Tree a) (Tree a) deriving (Read, Show) 
 
 -- define a function treeEmpty tree, which returns where the binary tree is empty
treeEmpty :: BTree a -> Bool
treeEmpty Empty = True
treeEmpty _ = False

 -- define a function treeRoot tree, which returns the root of the binary tree 
treeRoot :: BTree a -> a
treeRoot Empty = error "the tree is empty"
treeRoot (Node val _ _) = val

 -- define a function treeCount tree, which returns the elements of a binary tree
treeCount :: (Num b) => BTree a -> b 
treeCount Empty = 0
treeCount (_ left right) = 1 + treeCount left + treeCount right


-- Exercises

-- EX. 1
-- Let us have the polymorphic algebraic type List.
data List a = Nil | a 'Cons' List a deriving (Read, Show)

-- Define recursively the following functions:
  -- a) mkList accepting a standard list and turning it into a list of type List
mkList :: [a] -> List
mkList [] = Nil 
mkList (x:xs) = Cons x (mkList xs)

  -- b) listEmpty, just like empty, but for a list of type List.
listEmpty :: List a -> Bool
listEmpty Nil = True
listEmpty _ = False
  
  -- c) listHead, just like head, but for a list of type List.
listHead :: List a -> a  
listHead Nil = error "no list"
listHead (x 'Cons' xs) = x

  -- d) listTail, just like tail, but for a list of type List.
listTail :: List a -> a 
listTail Nil = error "no list" 
listTail (x 'Cons' xs) = xs

  -- e) listMap, just like map, but for a list of type List.
listMap :: (a -> a) -> List a -> List a
listMap f Nil = Nil
listMap f (x 'Cons' xs) = (f x) 'Cons' (listMap f xs)

  -- f) listFilter, just like filter, but for a list of type List.
listFilter :: (a -> Bool) -> List a -> List accepting
listFilter _ Nil = Nil
listFilter f (x 'Cons' xs) = if (f x) then x 'Cons' (listFilter f xs) else listFilter f xs


-- EX. 2
-- write a function finding the sum of all values of a tree
sumTreeValues :: Num a => BTree a -> a
sumTreeValues Empty = 0
sumTreeValues (Node n (tr1) (tr2)) = n + sumTreeValues tr1 + sumTreeValues tr2

-- write a function finding the depth of a tree
-- hint: you need to find the longer way of the two (as we have a binary tree)
findTreeDepth :: (Num b, Ord b) => BTree a -> b
findTreeDepth Empty = 0
findTreeDepth (Node n tr1 tr2) = 1 + max (findTreeDepth tr1) (findTreeDepth tr2)

-- write a function counting the leaves of a tree
countTreeLeaves :: (Num b) => BTree a -> b
countTreeLeaves Empty = 0
countTreeLeaves (Node n (Empty) (Empty)) = 1
countTreeLeaves (Node n tr1 tr2) = countTreeLeaves tr1 + countTreeLeaves tr2

-- write a function finding an value in a tree
findTreeElem :: Eq a => a -> BTree a -> Bool
findTreeElem _ Empty = False
findTreeElem el (Node n tr1 tr2) = (el == value) || findTreeElem el tr1 || findTreeElem el tr2

-- write a function returning a list with a tree's values
findTreeNodes :: BTree a -> [a]
findTreeNodes Empty = []
findTreeNodes (Node n tr1 tr2) = (findTreeNodes tr1) ++ [n] ++ (finTreeNodes tr2) 
