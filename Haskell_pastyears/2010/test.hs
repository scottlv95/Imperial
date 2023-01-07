import GHC (getInfo)
data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Show)

------------------------------------------------------

isPrefix :: String -> String -> Bool
-- isPrefix "" _ = True
-- isPrefix _ "" = False
-- isPrefix (x:xs) (y:ys) = (x == y) && isPrefix xs ys
isPrefix x y = x == take (length x) y 

removePrefix :: String -> String -> String
removePrefix  = drop . length 

suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes s 
  = s : suffixes (tail s)

isSubstring :: String -> String -> Bool
isSubstring subs x
  = any (isPrefix subs) (suffixes x) 

findSubstrings :: String -> String -> [Int]
-- findSubstrings subs x
--   = findHelper subs (suffixes x) [] 0

-- findHelper :: String -> [String] ->[Int] -> Int -> [Int]
-- findHelper subs [] ret count = ret 
-- findHelper subs (x:xs) ret count 
--  | isPrefix subs x = findHelper subs xs (ret++[count]) (count+1)
--  | otherwise  = findHelper subs xs ret (count+1)
 
findSubstrings subs x 
 = filter (isPrefix subs . (suffixes x !!)) [1..(length x - length subs)] 


------------------------------------------------------

getIndices :: SuffixTree -> [Int]
getIndices (Leaf x) = [x]  
-- getIndices (Node ((s,tree):nodes)) = getIndices tree ++ getIndices (Node nodes)
getIndices (Node trees) = concatMap (getIndices . snd) trees

partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition [] [] = ([], [], [])
partition [] b = ([], [], b)
partition a [] = ([], a, [])
partition a b = partition' a b []


partition':: Eq a => [a] -> [a] -> [a] -> ([a],[a],[a])
partition' [] b  c= (c, [], b)
partition' a [] c= (c, a, [])
partition' aas@(a:as) bbs@(b:bs) common 
 | a /= b = (common, aas, bbs) 
 | otherwise = partition' as bs (common++[a])

helperFind :: String -> SuffixTree -> [SuffixTree]
-- if have residue then dont return
helperFind str (Node [])
 = []
helperFind str t@(Leaf i) 
 | null str = [t]
 | otherwise = []
helperFind str t@(Node st@((s,tree):strees))
 | null str = [t]
 | null residue = [tree]
 | null common = helperFind str (Node strees)
--  | otherwise = helperFind residue tree ++ helperFind str (Node strees)
 | otherwise = concatMap (helperFind residue . snd) st 
  where (common, residue, _) = partition str s 

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' str t
 | null ans = []
 | otherwise = getIndices (head ans)
  where ans = helperFind str t 

------------------------------------------------------

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (s,n) (Node []) = Node [(s,Leaf n)]
insert (s,n) (Node ((a, t):strees))
  | null p = Node ((a,t):insertedTree)
  | p == a = Node ((a,insertedTree2):strees)
  | p /= a = Node ((p,newNode):strees)
  where (p, residue, ap) = partition s a   
        Node insertedTree = insert (s,n) (Node strees)
        insertedTree2 = insert (residue,n) t 
        newNode = Node [(residue,Leaf n),(ap,t)]
-- This function is given
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

------------------------------------------------------
-- Part IV

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring 
  = undefined

------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1 
  = "banana"

s2 :: String
s2 
  = "mississippi"

t1 :: SuffixTree
t1 
  = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 
  = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]

