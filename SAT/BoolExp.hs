{-# LANGUAGE GADTs, EmptyDataDecls  #-}
{-# LANGUAGE TypeFamilies #-}
-- | Boolean Expression module to build CNF from arbitrary expressions
--
-- TODO: 
--
-- * redefine as a "Functor"
--
module SAT.BoolExp where

-- | BoolForm型
data BoolForm =
  Lit Int                       -- ^ a literal in BoolForm
  | Cls [BoolForm]              -- ^ a clause in BoolForm
  | Cnf [BoolForm]              -- ^ well-structured BoolForm
    deriving (Eq, Show)
      
-- | negate "BoolForm"
-- 
-- >>> let (l1, l2, l3, l4) = (Lit 1, Lit 2, Lit 3, Lit 4)
-- >>> let (c1, c2, c3) = (l1 -|- l2, l2 -|- l3, l3 -|- l4 )
-- >>> asList $ neg l1
-- [[-1]]
--
-- >>> asList $ neg c1
-- [[-1],[-2]]
--
-- >>> let cnf = c1 -&- (c2 -&- c3)
-- >>> asList cnf
-- [[1,2],[2,3],[3,4]]
--
-- >>> asList $ neg cnf
-- [[-1,-2,-3],[-1,-2,-4],[-1,-3,-3],[-1,-3,-4],[-2,-2,-3],[-2,-2,-4],[-2,-3,-3],[-2,-3,-4]]
--
neg :: BoolForm -> BoolForm
neg (Lit n) = Lit $ negate n
neg (Cls ls) = Cnf [Cls [(neg l)] | l <- ls]
neg cnf@(Cnf _) = Cnf [Cls (map (Lit . negate) l) | l <- combinationOf lits]
  where
    lits = asList cnf

-- | Disjunction constructor
--
-- >>> let c1 = (Lit 3 -|- Lit 4)
-- >>> c1
-- Cls [Lit 3,Lit 4]
--
-- >>> asList c1
-- [[3,4]]
--  
-- >>> asList (c1 -|- Lit (-1))
-- [[3,4,-1]]
--  
(-|-) l1@(Lit a) l2@(Lit b) = Cls [l1, l2]
(-|-) (Cls ls) lit@(Lit l) = Cls $ ls ++ [lit]
(-|-) (Cls as) (Cls bs) = Cls $ as ++ bs
(-|-) (Cnf cnf) l@(Lit _) = Cnf [ c -|- l | c <- cnf]
(-|-) (Cnf cnf) c@(Cls _) = Cnf [ c' -|- c | c' <- cnf]
(-|-) cnf@(Cnf _) (Cnf cs) = foldr (\c cnf -> cnf -|- c) cnf cs
(-|-) a b = b -|- a

-- | Conjunction constructor
--
-- >>> let (l1, l2, l3, l4) = (Lit 1, Lit 2, Lit 3, Lit 4)
-- >>> let (n1, n2, n3, n4) = (Lit (-1), Lit (-2), Lit (-3), Lit (-4))
-- >>> asList $ l3 -&- n2
-- [[3],[-2]]
--
-- >>> asList $ l3 -|- (l1 -&- l2)
-- [[1,3],[2,3]]
--
(-&-) :: BoolForm -> BoolForm -> BoolForm
(-&-) a@(Lit _) b@(Lit _) = Cnf [Cls [a], Cls [b]]
(-&-) c@(Cls _) l@(Lit _) = Cnf [c, Cls [l]]
(-&-) a@(Cls _) b@(Cls _) = Cnf [a, b]
(-&-) (Cnf cnf) l@(Lit _) = Cnf $ Cls [l]: cnf
(-&-) (Cnf cnf) c@(Cls _) = Cnf $ c : cnf
(-&-) a b = b -&- a

-- | converts "CNF" to "[[Int]]"
asList :: BoolForm -> [[Int]]
asList a@(Lit i) = [[i]]
asList c@(Cls _) = [asClauseList c]
asList (Cnf cnf) = [asClauseList c | c <- cnf]

-- | converts "Clause" to "[[Int]]"
asClauseList :: BoolForm -> [Int]
asClauseList (Cls ls) = map asLiteral ls

-- | converts "Literal" to "[Int]"
-- asList exp = asList $ ToSAT exp
asLiteral (Lit i) = i

-- | bulid all combination of [Int]
--
-- >>> combinationOf [[1,2], [1,2]]
-- [[1,1],[1,2],[2,1],[2,2]]
--
-- >>> combinationOf [[1], [3,4], [5,6,7]]
-- [[1,3,5],[1,3,6],[1,3,7],[1,4,5],[1,4,6],[1,4,7]]
--
combinationOf :: [[Int]] -> [[Int]]
combinationOf [] = [] 
combinationOf [x] = [[a] | a <-x]
combinationOf (x:l) = concat [map (a :) l' | a <- x, let l' = combinationOf l]

