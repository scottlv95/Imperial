module Alloc where

import Data.Maybe
import Data.List

import Types
import Examples

------------------------------------------------------
--
-- Part I
--
count :: Eq a => a -> [a] -> Int
count letter lst
  = length (filter (letter == ) lst)


degrees :: Eq a => Graph a -> [(a, Int)]
degrees ([],edges)
 = []
degrees (x:xs, edges) 
  = (x,count x (flattenEdges edges)) : degrees (xs,edges)

flattenEdges :: [Edge a] -> [a] 
flattenEdges []
 = []
flattenEdges ((e1,e2):edges)
 = [e1,e2] ++ flattenEdges edges

neighbours :: Eq a => a -> Graph a -> [a]
neighbours x (_, [])
 = []
neighbours x (n, (e1,e2):edges) 
  | x == e1 = e2 : neighbours x (n, edges) 
  | x == e2 = e1 : neighbours x (n, edges) 
  | otherwise = neighbours x (n, edges)

removeNode :: Eq a => a -> Graph a -> Graph a
removeNode node (xs, edges)
  = (newXs, newEdges)
    where newXs = filter (node /=) xs
          newEdges = filter (elim node) edges

elim :: Eq a => a -> (a,a) -> Bool
elim x (a,b) 
 | x == a || x == b = False
 | otherwise = True
------------------------------------------------------
--
-- Part II
--

minDegree :: [(a,Int)] -> (a,Int) -> (a,Int)
minDegree [] (minEdge, minDegree)
 = (minEdge,minDegree)
minDegree ((edge,degree):eds) (e',d')
 | degree < d' = minDegree eds (edge, degree) 
 | otherwise = minDegree eds (e',d')


colourGraph :: (Ord a, Show a) => Int -> Graph a -> Colouring a
colourGraph _ ([],_)
  = [] 
colourGraph k g
 = (n,c) : cMap
   where (n,_)         = minDegree degreesG (head degreesG)
         cMap = colourGraph k g'
         neighboursOfN = neighbours n g 
         degreesG      = degrees g
         g'            = removeNode n g
         maybeColourOfNeighbours = map (`lookup` cMap) neighboursOfN
         colourOfNeighbours = map (fromMaybe 0) maybeColourOfNeighbours
         c = if null [x | x <- [1..k], x `notElem` colourOfNeighbours ] 
             then  0
             else  head [x | x <- [1..k], x `notElem` colourOfNeighbours ] 
         



------------------------------------------------------
--
-- Part III
--
buildIdMap :: Colouring Id -> IdMap
buildIdMap c
  = ("return","return") : map mapColor c

mapColor :: (Id,Colour) -> (Id,Id)
mapColor (a,c) 
 | c == 0 = (a,a)
 | otherwise = (a,"R"++show c)

buildArgAssignments :: [Id] -> IdMap -> [Statement]
buildArgAssignments xxs idmap
  = map (\x -> Assign (lookUp x idmap) (Var x)) xxs  

renameExp :: Exp -> IdMap -> Exp
-- Pre: A precondition is that every variable referenced in 
-- the expression is in the idMap. 
renameExp (Const i) idmap = Const i
renameExp (Var i) idmap = Var (lookUp i idmap)
renameExp (Apply op e1 e2) idmap= Apply op (renameExp e1 idmap) (renameExp e2 idmap)

renameBlock :: Block -> IdMap -> Block
-- Pre: A precondition is that every variable referenced in 
-- the block is in the idMap. 
renameBlock b idmap 
  = map (`renameStatement` idmap) (filter (`filterSame` idmap) b)

filterSame :: Statement -> IdMap -> Bool
filterSame (Assign id exp) idmap 
 | renameExp (Var id) idmap == renameExp exp idmap = False
 | otherwise = True
filterSame _ _ = True

renameStatement :: Statement -> IdMap -> Statement
renameStatement (Assign id exp) idmap 
 = Assign newId (renameExp exp idmap)
   where Var newId = renameExp (Var id) idmap
renameStatement (If exp b1 b2) idmap = If (renameExp exp idmap)
                                          (renameBlock b1 idmap)
                                          (renameBlock b2 idmap)
renameStatement (While exp b) idmap = While (renameExp exp idmap) (renameBlock b idmap)

renameFun :: Function -> IdMap -> Function
renameFun (f, as, b) idMap
  = (f, as, buildArgAssignments as idMap ++ renameBlock b idMap)

-----------------------------------------------------
--
-- Part IV
--
buildIG :: [[Id]] -> IG
buildIG x
  = (buildNode x, nub (concatMap buildId x))

buildId :: [Id] -> [Edge Id]
buildId xs = [(x,y)|x<-xs,y<-xs,x<y]

buildNode :: [[Id]] -> [Id]
buildNode x = nub (concat x)

-----------------------------------------------------
--
-- Part V
--
liveVars :: CFG -> [[Id]]
liveVars 
  = undefined

buildCFG :: Function -> CFG
buildCFG 
  = undefined


