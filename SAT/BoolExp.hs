{-# LANGUAGE FlexibleInstances #-}
-- | Boolean Expression module to build CNF from arbitrary expressions
--
-- TODO: 
--
-- * redefine as a "Functor"
--
module SAT.BoolExp 
       ( 
         -- * Class & Type
         BoolComponent (..)
       , BoolForm (..)
         -- * Expression contructors
       , (-|-)
       , (-&-)
       , (-!-)
       , (->-)
       , neg
         -- * List Operation
       , disjunctionOf
       , conjunctionOf
         -- * Convert function
       , asList
       , asLatex
       )
       where
import Data.List (intercalate)

class BoolComponent a where
  -- | lift to BoolForm
  toBF :: a -> BoolForm

-- | disjunction constructor
--
-- >>> let c1 = "3" -|- "4"
-- >>> c1
-- Cls [Lit 3,Lit 4]
--
-- >>> asList c1
-- [[3,4]]
--  
-- >>> asList (c1 -|- Lit (-1))
-- [[3,4,-1]]
--  
-- >>> let c1 = "1" -&- "2"
-- >>> let c2 = "3" -&- "4"
-- >>> asList $ c1 -|- c2
-- [[1,3],[2,3],[1,4],[2,4]]
--  
(-|-) :: (BoolComponent a, BoolComponent b) => a -> b -> BoolForm
a -|- b = toBF a -||- toBF b

-- | conjunction constructor
--
-- >>> asList $ "3" -&- "-2"
-- [[3],[-2]]
--
-- >>> asList $ "3" -|- ("1" -&- "2")
-- [[1,3],[2,3]]
--
(-&-) :: (BoolComponent a, BoolComponent b) => a -> b -> BoolForm
a -&- b = toBF a -&&- toBF b

-- | negate a form
-- 
-- >>> let (c1, c2, c3) = ("1" -|- "2", "2" -|- "3", "3" -|- "4" )
-- >>> asList $ neg c1
-- [[-1],[-2]]
--
-- >>> let cnf = c1 -&&- (c2 -&&- c3)
-- >>> asList cnf
-- [[1,2],[2,3],[3,4]]
--
-- >>> asList $ neg cnf
-- [[-1,-2,-3],[-1,-2,-4],[-1,-3,-3],[-1,-3,-4],[-2,-2,-3],[-2,-2,-4],[-2,-3,-3],[-2,-3,-4]]
--
neg :: (BoolComponent a) => a -> BoolForm
neg a = negBF $ toBF a

(-!-) :: (BoolComponent a) => a -> BoolForm
(-!-) = neg

-- | implication
--
-- >>> asList ("1" ->- "2")
-- [[-1,2]]
--
-- >>> asList $ ("1" -&- "2") ->- ("3" -|- "4")
-- [[-1,-2,3,4]]
--
(->-) :: (BoolComponent a, BoolComponent b) => a -> b -> BoolForm
a ->- b = neg (toBF a) -|- toBF b

instance BoolComponent Int where
  toBF a = Lit a

instance BoolComponent [Char] where
  toBF a = Lit (read a::Int)

-- | BoolForm型
data BoolForm =
  Lit Int                       -- ^ a literal in BoolForm
  | Cls [BoolForm]              -- ^ a clause in BoolForm
  | Cnf [BoolForm]              -- ^ well-structured BoolForm
    deriving (Eq, Show)
      
instance BoolComponent BoolForm where
  toBF = id

(-||-) l1@(Lit a) l2@(Lit b) = Cls [l1, l2]
(-||-) (Cls ls) lit@(Lit l) = Cls $ ls ++ [lit]
(-||-) (Cls as) (Cls bs) = Cls $ as ++ bs
(-||-) (Cnf cnf) l@(Lit _) = Cnf [ c -||- l | c <- cnf]
(-||-) (Cnf cnf) c@(Cls _) = Cnf [ c' -||- c | c' <- cnf]
(-||-) cnf@(Cnf _) (Cnf cs) = foldr (\c cnf -> cnf -||- c) cnf cs
(-||-) a b = b -||- a

-- | merge [BoolForm] by '(-|-)'
disjunctionOf :: [BoolForm] -> BoolForm
disjunctionOf (x:l) = foldr (-|-) x l

(-&&-) a@(Lit _) b@(Lit _) = Cnf [Cls [a], Cls [b]]
(-&&-) c@(Cls _) l@(Lit _) = Cnf [c, Cls [l]]
(-&&-) a@(Cls _) b@(Cls _) = Cnf [a, b]
(-&&-) (Cnf cnf) l@(Lit _) = Cnf $ Cls [l]: cnf
(-&&-) (Cnf cnf) c@(Cls _) = Cnf $ c : cnf
(-&&-) cnf@(Cnf _) (Cnf cs) = foldr (\c cnf -> cnf -&&- c) cnf cs
(-&&-) a b = b -&&- a

-- | merge [BoolForm] by '(-&-)'
conjunctionOf :: [BoolForm] -> BoolForm
conjunctionOf (x:l) = foldr (-&-) x l

negBF (Lit n) = Lit $ negate n
negBF (Cls ls) = Cnf [Cls [(negBF l)] | l <- ls]
negBF cnf@(Cnf _) = Cnf [Cls (map (Lit . negate) l) | l <- combinationOf lits]
  where
    lits = asList cnf

-- | converts a BoolForm to "[[Int]]"
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

-- | make latex string from CNF
--
-- >>> asLatex $ "3" -|- "4" 
-- "\\begin{displaymath}\n( x_{3} \\vee x_{4} )\n\\end{displaymath}\n"
--
asLatex :: BoolForm -> String
asLatex b = beg ++ s ++ end
  where
    beg = "\\begin{displaymath}\n"
    end = "\n\\end{displaymath}\n"
    s = intercalate " \\wedge " [ makeClause c | c <- asList b]
    makeClause c = "(" ++ intercalate "\\vee" [makeLiteral l | l <- c] ++ ")"
    makeLiteral l
      | 0 < l = " x_{" ++ show l ++ "} "
      | otherwise = " \\neg " ++ "x_{" ++ show (negate l) ++ "} "
