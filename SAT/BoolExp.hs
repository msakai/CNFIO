{-# LANGUAGE BangPatterns, FlexibleInstances #-}
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
import Data.List (foldl', intercalate, sortBy, sort, nub)
import Data.Ord (comparing)

class BoolComponent a where
  -- | lift to BoolForm
  toBF :: a -> BoolForm
  fromBF :: BoolForm -> a
  fromBF = undefined

-- | BoolForm型
data BoolForm =
  Lit Int                       -- ^ a literal in BoolForm
  | Cls [BoolForm]              -- ^ a clause in BoolForm
  | Cnf [BoolForm]              -- ^ well-structured BoolForm
    deriving (Eq, Show)

instance Ord BoolForm where
  compare (Lit a) (Lit b) = comparing abs a b
  compare (Cls a) (Cls b) = compareAbsList (map unLit a) (map unLit b)
  compare (Cnf a) (Cnf b) = compare a b

unLit (Lit n) = n
unLit _ = error "tried to get Int value from un-Lit"

compareAbsList :: [Int] -> [Int] -> Ordering
compareAbsList [] [] = EQ
compareAbsList [] _ = LT
compareAbsList _ [] = GT
compareAbsList (a:l1) (b:l2)
  | val == EQ = compareAbsList l1 l2
  | otherwise = val
  where
    val = comparing abs a b

canonize :: BoolForm -> BoolForm
canonize l@(Lit _) = l
canonize (Cls ls) = Cls . nub $ sort ls
canonize (Cnf cs) = Cnf . filter toto . nub . sort $ map canonize cs
  where
    toto (Cls xs) = not $ any (\x -> elem (negate x) l) l
      where
        l = map unLit xs

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
-- [[-1,3,4]]
--  
-- >>> let c1 = "1" -&- "2"
-- >>> let c2 = "3" -&- "4"
-- >>> asList $ c1 -|- c2
-- [[1,3],[1,4],[2,3],[2,4]]
--  
(-|-) :: (BoolComponent a, BoolComponent b) => a -> b -> BoolForm
a -|- b = canonize $ toBF a -||- toBF b

-- | conjunction constructor
--
-- >>> asList $ "3" -&- "-2"
-- [[-2],[3]]
--
-- >>> asList $ "3" -|- ("1" -&- "2")
-- [[1,3],[2,3]]
--
(-&-) :: (BoolComponent a, BoolComponent b) => a -> b -> BoolForm
a -&- b =toBF a -&&- toBF b

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
-- [[-1,-2,-3],[-1,-2,-4],[-1,-3],[-1,-3,-4],[-2,-3],[-2,-3,-4],[-2,-4]]
--
neg :: (BoolComponent a) => a -> BoolForm
neg a = canonize $ negBF $ toBF a

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
a ->- b = canonize $ neg (toBF a) -|- toBF b

instance BoolComponent Int where
  toBF a = Lit a

instance BoolComponent [Char] where
  toBF a = Lit (read a::Int)
      
instance BoolComponent BoolForm where
  toBF = id

{-
-- This causes 'Constraint is no smaller than the instance head'
-- So if you want to use your Enum in forms, you declase it as 'BoolComponent'
instance (Enum e) => BoolComponent e where
  toBF = Lit . fromEnum
-}

(-||-) l1@(Lit a) l2@(Lit b) = Cls $ sortLit [l1, l2]
(-||-) (Cls ls) lit@(Lit l) = Cls . sortLit $ ls ++ [lit]
(-||-) (Cls as) (Cls bs) = Cls . sortLit $ as ++ bs
(-||-) (Cnf cnf) l@(Lit _) = Cnf [ c -||- l | c <- cnf]
(-||-) (Cnf cnf) c@(Cls _) = Cnf [ c' -||- c | c' <- cnf]
(-||-) cnf@(Cnf _) (Cnf cs) = foldl' (-&-) x $ l
  where
    (x:l) = [cnf -||- c | c <- cs]
(-||-) a b = b -||- a

sortLit :: [BoolForm]  -> [BoolForm]
sortLit = sortBy (comparing (abs . unLit))

-- | merge [BoolForm] by '(-|-)'
disjunctionOf :: [BoolForm] -> BoolForm
disjunctionOf (x:l) = foldl' (-|-) x l

(-&&-) a@(Lit _) b@(Lit _) = Cnf [Cls [a], Cls [b]]
(-&&-) c@(Cls _) l@(Lit _) = Cnf [c, Cls [l]]
(-&&-) a@(Cls _) b@(Cls _) = Cnf [a, b]
(-&&-) (Cnf cnf) l@(Lit _) = Cnf $ Cls [l]: cnf
(-&&-) (Cnf cnf) c@(Cls _) = Cnf $ c : cnf
(-&&-) cnf@(Cnf _) (Cnf cs) = foldl' (\c cnf -> cnf -&&- c) cnf cs
(-&&-) a b = b -&&- a

-- | merge [BoolForm] by '(-&-)'
conjunctionOf :: [BoolForm] -> BoolForm
conjunctionOf (x:l) = foldr (-&-) x l

negBF (Lit n) = Lit $ negate n
negBF (Cls ls) = Cnf [Cls [(negBF l)] | l <- ls]
negBF cnf@(Cnf _) = Cnf [Cls $ map (Lit . negate) l | l <- combinationOf lits]
  where
    lits = asList cnf

-- | converts a BoolForm to "[[Int]]"
asList :: BoolForm -> [[Int]]
asList !a@(Lit i) = [[i]]
asList !c@(Cls _) = [asClauseList c]
asList !cnf@(Cnf _) = [asClauseList c | c <- cnf']
  where
    (Cnf cnf') = canonize cnf

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
