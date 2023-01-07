module Radix where

import Prelude hiding (and, or)

data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a)
            deriving (Eq, Show)

type IntTree = Tree Int

data Bit = Zero | One
         deriving (Eq, Show)

type RadixTree = Tree Bit

type BitString = [Int]

--------------------------------------------------------------------------

buildIntTree :: [Int] -> IntTree
buildIntTree
  = foldr add Empty
  where
    add x Empty
      = Leaf x
    add x (Leaf y)
      = add x (Node y Empty Empty)
    add x t@(Node y l r)
      | x == y    = t
      | x < y     = Node y (add x l) r
      | otherwise = Node y l (add x r)

--------------------------------------------------------------------------

a, m :: Integer
m = 1073741824
a = 16387

rand :: Integer -> [Double]
rand s
  = fromInteger s / fromInteger m : rand s' where s' = (s * a) `mod` m

randomInts :: Int -> Int -> Integer -> [Int]
randomInts m n s
  = take m (map (round . (+1) . (* (fromIntegral n))) (rand s))

rs :: [Int]
rs = randomInts 1000 500 765539

--------------------------------------------------------------------------
-- Pre (universal): all integers are non-negative

sizeIT :: IntTree -> Int
sizeIT Empty = 1
sizeIT (Leaf _)= 4
sizeIT (Node n left right)
 = 12 + sizeIT left + sizeIT right


sizeRT :: RadixTree -> Int
sizeRT (Leaf _) = 1
sizeRT (Node n left right)
 = 8 + sizeRT left + sizeRT right


--
-- NOTE: The above import Prelude hiding (and, or) 
-- will allow you to name these two functions without
-- a name clash
--
and :: Bit -> Bit -> Bit
and a b
  | a == One && b == One = One
  | otherwise = Zero

or :: Bit -> Bit -> Bit
or a b
  | a == Zero && b == Zero = Zero
  | otherwise = One 

bin :: Int -> [Int] -> [Int]
bin 0 [] = [0]
bin 0 xs = xs
bin 1 xs = 1:xs
bin k xs
 = bin d (r:xs)
       where r = k `mod` 2 
             d = k `div` 2

binary :: Int -> BitString
binary k
  = bin k [] 

insert :: BitString -> RadixTree -> RadixTree
insert [] (Node _ left right) = Node One left right
insert [] (Leaf _) = Leaf One
insert (b:bs) (Leaf x) 
  | b == 0 = Node x (insert bs (Leaf Zero) ) (Leaf Zero)
  | b == 1 = Node x (Leaf Zero) (insert bs (Leaf Zero))
insert (b:bs) (Node x left right)
 | b == 0 = Node x (insert bs left) right 
 | b == 1 = Node x left (insert bs right) 

buildRadixTree :: [Int] -> RadixTree
buildRadixTree = foldr (insert . binary) (Leaf Zero)



member :: Int -> RadixTree -> Bool
member = member' . binary


member' :: BitString -> RadixTree -> Bool
member' [] rdxTree
 | rdxTree == Leaf Zero = False
 | rdxTree == Leaf One  = True 
member' [] (Node Zero _ _ ) = False
member' [] (Node One _ _ ) = True
member' _ (Leaf _) = False

member' (x:xs) (Node _ left right)
 | x == 1 = member' xs right
 | x == 0 = member' xs left


union :: RadixTree -> RadixTree -> RadixTree
union a b 
 = buildRadixTree [ x | x <- [0..100] , member x a || member x b]


intersection :: RadixTree -> RadixTree -> RadixTree
intersection a b 
 = buildRadixTree [ x | x <- [0..100] , member x a && member x b]
   

-- CONCLUSION: The break-even point is xxx.

-----------------------------------------------------------------------------
-- Some test trees...

figure :: RadixTree
figure
  = Node Zero (Leaf One)
               (Node One (Leaf Zero)
                          (Node One (Node Zero (Leaf One)
                                                 (Leaf Zero))
                                     (Leaf One)))

t1 :: IntTree
t1 = Node 20 (Node 8 Empty
                     (Node 12 Empty
                              Empty))
             Empty

t2 :: RadixTree
t2 = Node Zero (Node Zero (Leaf One)
                            (Node One (Leaf Zero) (Leaf One)))
                (Leaf One)