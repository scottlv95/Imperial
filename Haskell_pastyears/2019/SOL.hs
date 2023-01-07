module SOL where

import Data.List
import Data.Maybe

import Types
import TestData

printF :: Formula -> IO()
printF
  = putStrLn . showF
  where
    showF (Var v)
      = v
    showF (Not f)
      = '!' : showF f
    showF (And f f')
      = "(" ++ showF f ++ " & " ++ showF f' ++ ")"
    showF (Or f f')
      = "(" ++ showF f ++ " | " ++ showF f' ++ ")"

--------------------------------------------------------------------------
-- Part I

-- 1 mark
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp n lst
 = head [ b | (a,b)<-lst, a == n]

-- 3 marks
vars :: Formula -> [Id]
vars (Var x) = [x]
vars (Not f) = vars f
vars (And f1 f2) = sort (nub (vars f1 ++ vars f2))
vars (Or f1 f2) = sort (nub (vars f1 ++ vars f2))

-- 1 mark
idMap :: Formula -> IdMap
idMap f 
  = zip (vars f) [1..]

--------------------------------------------------------------------------
-- Part II

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

-- 4 marks
toNNF :: Formula -> NNF
toNNF (Not (Not x)) = toNNF x
toNNF (Not (And f1 f2)) = Or (toNNF (Not f1)) (toNNF (Not f2))
toNNF (Not (Or f1 f2 )) = And (toNNF(Not f1)) (toNNF(Not f2))
toNNF (And f1 f2) = And (toNNF f1) (toNNF f2) 
toNNF (Or f1 f2) = Or (toNNF f1) (toNNF f2) 
toNNF f = f

-- 3 marks
toCNF :: Formula -> CNF
toCNF f = toCNFHelper nnf
          where nnf = toNNF f 

toCNFHelper :: NNF -> CNF 

toCNFHelper (Or f1 f2) = distribute (toCNFHelper f1) (toCNFHelper f2)
toCNFHelper (And f1 f2) = And (toCNFHelper f1) (toCNFHelper f2)
toCNFHelper f = f

-- 4 marks
flatten :: CNF -> CNFRep
flatten f = flattenHelper f (idMap f)

flattenHelper :: CNF -> IdMap -> CNFRep
flattenHelper (Var x) table = [[lookUp x table]]
flattenHelper (Not (Var x)) table = [[- lookUp x table]]
flattenHelper (Or f1 f2) table = [concat (flattenHelper f1 table) ++ concat (flattenHelper f2 table)]
flattenHelper (And f1 f2) table = flattenHelper f1 table ++ flattenHelper f2 table 

--------------------------------------------------------------------------
-- Part III

-- 5 marks
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits x = propUnitHelper x [] 

filterFirst :: CNFRep -> (CNFRep,[Int]) 
filterFirst [] = ( [] , [] ) 
filterFirst (x:xs) 
 | length x == 1 = (xs,x) 
 | otherwise = (x:xs',k) 
               where (xs',k) = filterFirst xs 

propagate :: CNFRep -> [Int] -> CNFRep 
propagate [] _ = []
propagate (x:xs) k 
 | head k `elem` x = propagate xs k
 | otherwise = (x \\ map (\x-> -x) k) : propagate xs k 

propUnitHelper :: CNFRep -> [Int] -> (CNFRep,[Int])
propUnitHelper [] k = ([],k)
propUnitHelper xxs@(x:xs) k 
 | not (null filtered) = propUnitHelper (propagate remain filtered) (k `union` filtered)
 | otherwise           = (xxs,k)
                         where (remain,filtered) = filterFirst xxs 
      



-- 4 marks
dp :: CNFRep -> [[Int]]
dp x = map (++ r) (dpHelp c [[]])
      where (c,r) = propUnits x
dpHelp :: CNFRep -> [[Int]] -> [[Int]]
dpHelp [] path = path 
dpHelp ([]:_) _ = []
dpHelp xxs@(x:xs) path 
  = nub (dpHelp t (map (++ r1) path) ++ dpHelp f (map (++ r2) path))
   where (t,r1) = propUnits ([head x] : xxs)
         (f,r2) = propUnits ([-head x]: xxs)
                --  dpHelp t ++ dpHelp f  
--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
-- allSat :: Formula -> [[(Id, Bool)]]
-- allSat f =
--           where d = dp flatten toCNF f