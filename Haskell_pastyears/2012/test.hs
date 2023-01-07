import Data.List
import Data.Maybe
import Data.Char (toUpper)
import Control.Monad.Trans.RWS (state)

type Id = String

type State = Int

type Transition = ((State, State), Id)

type LTS = [Transition]

type Alphabet = [Id]

data Process = STOP | Ref Id | Prefix Id Process | Choice [Process] 
             deriving (Eq, Show)

type ProcessDef = (Id, Process)

type StateMap = [((State, State), State)]

------------------------------------------------------
-- PART I

lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: The item is in the table
lookUp x n
  = fromJust (lookup x n)

states :: LTS -> [State]
states l
  = nub (concatMap (\((a,b),letters)-> [a,b]) l)

transitions :: State -> LTS -> [Transition]
transitions s 
  = filter (\((a,_),words)-> a==s ) 

alphabet :: LTS -> Alphabet
alphabet l
  = nub (map snd l) 

------------------------------------------------------
-- PART II

actions :: Process -> [Id]
actions = nub . actions' 

actions' :: Process -> [Id]
actions' STOP = []
actions' (Ref _) = [] 
actions' (Prefix id p) = id : actions p
actions' (Choice p) = concatMap actions p

accepts :: [Id] -> [ProcessDef] -> Bool
--Pre: The first item in the list of process definitions is
--     that of the start process.
accepts = accepts'

accepts':: [Id] -> [ProcessDef] -> Bool
accepts' [] _ = True
accepts' (id:ids) ((i,Prefix p processes):xs)
 = (id==p) && accepts' ids ((i,processes):xs)
accepts' (id:ids) ((i,STOP):xs) 
 = False
accepts' ids ((i,Choice cs):xs)
 = any (\x -> accepts' ids ((i,x):xs)) cs
accepts' ids ((word,Ref refs):xs) -- Ref case
 = accepts' ids ((refs,p):xs)
   where p = lookUp refs xs  



------------------------------------------------------
-- PART III

composeTransitions :: Transition -> Transition 
                  -> Alphabet -> Alphabet 
                  -> StateMap 
                  -> [Transition]
--Pre: The first alphabet is that of the LTS from which the first transition is
--     drawn; likewise the second.
--Pre: All (four) pairs of source and target states drawn from the two transitions
--     are contained in the given StateMap.
composeTransitions ((s,t),a) ((s',t'),a') x x' statemap
  | a == a' = [((lookUp (s,s') statemap, lookUp (t,t') statemap), a)] 
  | a `elem` x' && a' `elem` x = []
  | a' `elem` x = [((lookUp (s,s') statemap, lookUp (t,s') statemap),a)]
  | a `elem` x' = [((lookUp (s,s') statemap, lookUp (s,t') statemap),a')]
  | otherwise = [((lookUp (s,s') statemap, lookUp (s,t') statemap),a'),
                 ((lookUp (s,s') statemap, lookUp (t,s') statemap),a)]

pruneTransitions :: [Transition] -> LTS
pruneTransitions ts 
  = nub (visit 0 [])
    where visit :: State -> [State] -> [Transition] 
          visit s visited 
            | s `notElem` visited = trans ++ concatMap (\((from,to),_)-> visit to (from:visited)) trans
            | otherwise = trans 
                                    where trans = transitions s ts
------------------------------------------------------
-- PART IV
-- clock play lts
compose :: LTS -> LTS -> LTS
compose l1 l2
 = pruneTransitions (concatMap (\x-> composeMultiple x l1 l2 statemap) states )
    where statemap = getAllState l1 l2 
          states = map fst statemap 

composeMultiple :: (State,State) -> LTS -> LTS -> StateMap ->[Transition]
composeMultiple (s,s') l1 l2 statemap
 = nub (concat [composeTransitions x y a a' statemap | x<- ts, y<-ts'])
    where ts = transitions s l1
          ts' = transitions s' l2
          a = alphabet l1
          a' = alphabet l2

getAllState :: LTS -> LTS -> [((State,State),State)]
getAllState l1 l2
 = zip [ (s,s') | s<-states l1, s'<-states l2] [0..]

------------------------------------------------------
-- PART V

buildLTS :: [ProcessDef] -> LTS
-- Pre: All process references (Ref constructor) have a corresponding
--      definition in the list of ProcessDefs.
buildLTS 
  = undefined



------------------------------------------------------
-- Sample process definitions...

vendor, clock, play, maker, user, p, q, switch, off, on :: ProcessDef

vendor 
  = ("VENDOR", Choice [Prefix "red"  (Prefix "coffee" (Ref "VENDOR")),
                       Prefix "blue" (Prefix "tea" (Ref "VENDOR")),
                       Prefix "off" STOP])

clock 
  = ("CLOCK", Prefix "tick" (Prefix "tock" (Ref "CLOCK")))

play 
  = ("PLAY", Choice [Prefix "think" (Prefix "move" (Ref "PLAY")), 
                     Prefix "end" STOP])

maker 
  = ("MAKER", Prefix "make" (Prefix "ready" (Ref "MAKER")))

user  
  = ("USER",  Prefix "ready" (Prefix "use" (Ref "USER")))

p = ("P", Prefix "a" (Prefix "b" (Prefix "c" STOP)))

q = ("Q",  Prefix "d" (Prefix "c" (Prefix "b" (Ref "Q"))))

switch 
  = ("SWITCH", Ref "OFF")

off 
  = ("OFF", Choice [Prefix "on" (Ref "ON")])

on  
  = ("ON",  Choice [Prefix "off" (Ref "OFF")])

------------------------------------------------------
-- Sample LTSs...

vendorLTS, clockLTS, playLTS, clockPlayLTS, makerLTS, userLTS, makerUserLTS, 
  pLTS, qLTS, pqLTS, switchLTS :: LTS

vendorLTS 
  = [((0,1),"off"),((0,2),"blue"),((0,3),"red"),((2,0),"tea"),((3,0),"coffee")]

clockLTS 
  = [((0,1),"tick"),((1,0),"tock")]

playLTS 
  = [((0,1),"end"),((0,2),"think"),((2,0),"move")]

clockPlayLTS 
  = [((0,1),"end"),((1,4),"tick"),((4,1),"tock"),((0,3),"tick"),
     ((3,4),"end"),((3,0),"tock"),((3,5),"think"),((5,3),"move"),
     ((5,2),"tock"),((2,0),"move"),((2,5),"tick"),((0,2),"think")]

makerLTS 
  = [((0,1),"make"),((1,0),"ready")]

userLTS 
  = [((0,1),"ready"),((1,0),"use")]

makerUserLTS 
  = [((0,2),"make"),((2,1),"ready"),((1,0),"use"),((1,3),"make"),((3,2),"use")]

pLTS 
  = [((0,1),"a"),((1,2),"b"),((2,3),"c")]

qLTS 
  = [((0,1),"d"),((1,2),"c"),((2,0),"b")]

pqLTS 
  = [((0,1),"d"),((1,4),"a"),((0,3),"a"),((3,4),"d")]

switchLTS 
  = [((0,1),"on"),((1,0),"off")]
