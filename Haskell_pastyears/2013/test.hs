type BinHeap a = [BinTree a]

data BinTree a = Node a Int (BinHeap a)
               deriving (Eq, Ord, Show)

--------------------------------------------------------------
-- PART I

value :: BinTree a -> a
value (Node v _ _) = v 

rank :: BinTree a -> Int
rank (Node _ r _) = r 

children :: BinTree a -> [BinTree a]
children (Node _ _ c) = c

combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
combineTrees a b
  | value a < value b = Node (value a ) (rank a+1) (b : children a ) 
  | otherwise = Node (value b ) (rank b+1) (a:children b)

--------------------------------------------------------------
-- PART II

extractMin :: Ord a => BinHeap a -> a
extractMin binheap
  = minimum (map value binheap)

mergeHeaps :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
mergeHeaps [] y = y
mergeHeaps x [] = x
mergeHeaps xxs@(x:xs) yys@(y:ys)
  | rank x < rank y = x : mergeHeaps xs yys
  | rank x > rank y = y : mergeHeaps xxs ys
  | rank y == rank x = mergeHeaps [combineTrees x y] (mergeHeaps xs ys)

insert :: Ord a => a -> BinHeap a -> BinHeap a
insert k 
  = mergeHeaps [Node k 0 []] 

deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin bh 
  = mergeHeaps newbh c  
   where c = (reverse . children) bt
         (bt, newbh) = removeMin bh


remove :: Eq a => a -> BinHeap a -> BinHeap a
remove k [] = []
remove k (bt:bts)
  | k == value bt = bts 
  | k /= value bt = bt: remove k bts

removeMin :: Ord a => BinHeap a -> (BinTree a, BinHeap a)
removeMin bh
  = (head (filter ((== k ) . value) bh), remove k bh)
    where k = extractMin bh
binSort :: Ord a => [a] -> [a]
binSort k  = binTransform bh
    where bh = foldr insert [] k

binTransform :: Ord a => BinHeap a -> [a]
binTransform [] = []
binTransform bh 
 = x: binTransform xs 
   where x = extractMin bh 
         xs = deleteMin bh 


--------------------------------------------------------------
-- PART III

toBinary :: BinHeap a -> [Int]
toBinary bh 
  = reverse (map (\x -> if x `elem` rs then 1 else 0)[0..head rs])
    where rs = reverse (binSort (map rank bh))
binarySum :: [Int] -> [Int] -> [Int]
binarySum x y = reverse (binarySumCarry (reverse x) (reverse y) 0)

binarySumCarry :: [Int] -> [Int] -> Int -> [Int]
binarySumCarry [] [] 0 = []
binarySumCarry [] [] c = [c]
binarySumCarry x [] 0 = x
binarySumCarry x [] c = binarySumCarry x [c] 0
binarySumCarry [] y c = binarySumCarry y [] c 
binarySumCarry (x:xs) (y:ys) c 
 =  r: binarySumCarry xs ys d
    where r = (c + x + y) `mod` 2
          d = (c+x+y) `div` 2
        

------------------------------------------------------
-- Some sample trees...

t1, t2, t3, t4, t5, t6, t7, t8 :: BinTree Int
-- Note: t7 is the result of merging t5 and t6

-- t1 to t4 appear in Figure 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []], 
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 and t6 are on the left of Figure 2; t7 is on the
-- right
t5 = Node 4 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []], 
               Node 7 0 []]

-- An additional tree...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- Some sample heaps...

h1, h2, h3, h4, h5, h6, h7 :: BinHeap Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 is shown in Figure 3...
h3 = [t1, t2, t4]

-- Two additional heaps, used below. They are shown
-- in Figure 4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 is the result of merging h4 and h5, shown in Figure 4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 is shown in Figure 5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]
