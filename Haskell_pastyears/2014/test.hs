import Data.Maybe
import Data.List
import GHC.Exception (underflowException)

data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int

data Label = C Char | Eps
           deriving (Eq, Ord, Show)

type Transition = (State, State, Label)

type Automaton = (State, [State], [Transition])

--------------------------------------------------------
-- showRE - this may be useful for testing

showRE :: RE -> String
showRE (Seq re re')
  = showRE re ++ showRE re'
showRE (Alt re re')
  = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)
  = showRE' re ++ "*"
showRE (Plus re)
  = showRE' re ++ "+"
showRE (Opt re)
  =  showRE' re ++ "?"
showRE re
  = showRE' re

showRE' Null
  = ""
showRE' (Term c)
  = [c]
showRE' (Alt re re')
  = showRE (Alt re re')
showRE' re
  = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Part I

lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: There is exactly one occurrence of the item being looked up.
lookUp k v
  = fromJust (lookup k v)


-- data RE = Null   |
--           Term Char |
--           Seq RE RE |
--           Alt RE RE |
--           Rep RE    |
--           Plus RE   |
--           Opt RE
--         deriving (Eq, Show)

simplify :: RE -> RE
simplify (Term c) = Term c
simplify Null = Null
simplify (Plus re) = Seq re (Rep re)
simplify (Opt re) = Alt re Null
simplify (Seq re1 re2) = Seq (simplify re1) (simplify re2)
simplify (Alt re1 re2) = Alt (simplify re1) (simplify re2)
simplify (Rep re) = Rep (simplify re) 

--------------------------------------------------------
-- Part II

startState :: Automaton -> State
startState (s,_,_)
  = s
terminalStates :: Automaton -> [State]
terminalStates (_,t,_)
  = t
transitions :: Automaton -> [Transition]
transitions (_,_,tran)
  = tran

isTerminal :: State -> Automaton -> Bool
isTerminal s a
  = s `elem` terminalStates a

transitionsFrom :: State -> Automaton -> [Transition]
transitionsFrom s a
  = filter (\(x,_,_) -> x == s) (transitions a)

labels :: [Transition] -> [Label]
labels trans
  = nub (filter (/=Eps) (map (\(_,_,l)->l) trans))

accepts :: Automaton -> String -> Bool
accepts a s   
  = accepts' 1 s

  where   accepts' :: State -> String -> Bool
          accepts' state s   
           | isTerminal state a && null s = True 
           | otherwise = any (try s) (transitions a)
           
          
          try :: String -> Transition -> Bool
          try xxs (_, t, Eps) = accepts' t xxs
          try xxs@(x:xs) (from, t, C c) 
           | null s = False 
           | x == c = accepts' t xs
           | otherwise = False
           
-------------------------------------------------------
-- Part III

makeNDA :: RE -> Automaton
makeNDA re
  = (1, [2], sort transitions)
  where
    (transitions, k) = make (simplify re) 1 2 3

make :: RE -> Int -> Int -> Int -> ([Transition], Int)
-- base case
make Null m n k = ([(m,n,Eps)], k)
make (Term c) m n k = ([(m,n,C c)], k)
make (Seq r1 r2) m n k = (tran1++[(k,k+1,Eps)]++tran2, k2)
                          where (tran1, k1) = make r1 m k (k+2)
                                (tran2, k2) = make r2 (k+1) n k1

make (Alt r1 r2) m n k = ([(m,k,Eps),(m,k+2,Eps)]++tran1++tran2++[(k+1,n,Eps),(k+3,n,Eps)], k2)
                         where (tran1, k1) = make r1 k (k+1) (k+4)
                               (tran2, k2) = make r2 (k+2) (k+3) k1

make (Rep r) m n k = ([(m,k,Eps),(m,n,Eps),(k+1,k,Eps),(k+1,n,Eps)] ++ tran1, k1)
                     where (tran1, k1) = make r k (k+1) (k+2)



--------------------------------------------------------
-- Part IV

type MetaState = [State]

type MetaTransition = (MetaState, MetaState, Label)

getFrontier :: State -> Automaton -> [Transition]
getFrontier s a@(start, termStates, trans) 
  | isTerminal s a = [(s,s,Eps)]
  | otherwise = withC ++ concatMap (`getFrontier` a) e
                where tfrom = transitionsFrom s a
                      withEPS = filter (\(from, to, label ) -> label == Eps) tfrom
                      withC = tfrom \\ withEPS
                      e = map (\(_,x,_)->x) withEPS

getFrontiers :: [State] -> Automaton -> [Transition] 
getFrontiers s a = concatMap (`getFrontier` a) s


groupTransitions :: [Transition] -> [(Label, [State])]
groupTransitions t
  = [ (l,[ to |(_,to,label) <- t , label == l ])|l<-labels t] --revisit 

makeDA :: Automaton -> Automaton
-- Pre: Any cycle in the NDA must include at least one non-Eps transition
makeDA a@(start, term, trans)
  = undefined 
    where ans = makeDA' [start] [] []
          root = getMetaStates [start]
          getMetaStates :: [State] -> MetaState 
          getMetaStates s = (sort . nub) [m | (m,_,_) <- getFrontiers s a ] 
          makeDA' :: [State] -> [MetaState] -> [MetaTransition] -> (MetaState, [MetaState], [MetaTransition])
          makeDA' s visited mtran 
           | metastates `elem` visited = (root, visited, mtran)
           | otherwise = extendDA metastates gts visited mtran 
                                      where gts = groupTransitions (getFrontiers s a)
                                            metastates = getMetaStates s 
                                            extendDA :: MetaState -> [(Label,[State])] -> [MetaState] -> [MetaTransition] -> (MetaState, [MetaState], [MetaTransition]) 
                                            extendDA ms gtss@(gt:gts) mss mts 
                                             =  foldl extend (ms, ms:mss, mts) gts 
                                                  where extend (ms, mss, mts) (label,metaStates)
                                                         = (ms, ts',(ms, r', label) :ms')
                                                           where (r',ts',ms') = makeDA' metaStates visited mtran

                     
              
--               where (groupTransitions . getFrontier) s 
--------------------------------------------------------
-- Test cases

reFigure, re1, re2, re3, re4, re5 :: RE
reFigure
  = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1
  = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2
  = Seq (Term 'x') (Rep (Term '\''))
re3
  = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4
  = Seq (Alt (Term 'a') Null) (Term 'a')
re5
  = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))

nd, nd' :: Automaton
nd = (1,[4],[(1,2,C 'a'),(1,3,C 'b'),(2,3,Eps),(2,4,C 'c')])

nd' = (1,[4],[(1,2,Eps),(1,3,C 'a'),(2,4,C 'a'),(2,4,C 'b'),
              (3,4,C 'b'),(3,4,Eps)])

da :: Automaton
da = (0,[3],[(0,1,C 'a'),(0,2,C 'a'),(0,2,C 'b'),(1,2,C 'a'),
             (1,3,C 'b'),(2,2,C 'a'),(2,1,C 'a'),(2,3,C 'b')])

re :: RE
re = Seq (Alt (Term 'a') (Term 'b')) (Seq (Rep (Term 'a')) (Term 'b'))

ndaFigure, nda1, nda2, nda3, nda4, nda5 :: Automaton
daFigure, da1, da2, da3, da4, da5 :: Automaton
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],
     [(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1
  = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2
  = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2
  = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3
  = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3
  = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4
  = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5
  = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])