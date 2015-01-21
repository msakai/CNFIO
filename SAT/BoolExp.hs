{-# LANGUAGE BangPatterns, FlexibleInstances, ViewPatterns, UndecidableInstances #-}
-- | Boolean Expression module to build CNF from arbitrary expressions
module SAT.BoolExp 
       ( 
         -- * Class & Type
         BoolComponent (..)
       , BoolForm (..)
       , renumber
       , tseitinNumber
       , maxRank
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

import Control.Applicative
import Data.List (foldl', intercalate, sortBy, sort, nub)
import Data.Ord (comparing)

tseitinBase :: Int
tseitinBase = 800000

class BoolComponent a where
  -- | lift to BoolForm
  toBF :: a -> BoolForm
  fromBF :: BoolForm -> a
  fromBF = undefined

-- | BoolForm型
data BoolForm = Cnf (Int, Int) [[Int]] -- ^ well-structured BoolForm
    deriving (Eq, Show)


isLiteral :: BoolForm -> Maybe Int
isLiteral (Cnf _ [[x]]) = Just x
isLiteral _ = Nothing

clausesOf cnf@(Cnf _ l)
  | Just x <- isLiteral cnf = []
  | otherwise = l

maxRank :: BoolForm -> Int
maxRank (Cnf (n, _) _) = n

tseitinNumber :: BoolForm -> Int
tseitinNumber (Cnf (m, n) _)
  | tseitinBase < n = n
  | otherwise = m

-- | renumbering
renumber :: Int -> BoolForm -> BoolForm
renumber base (Cnf (m, n) l) = Cnf (m, if x < tseitinBase then 0 else x) l'
  where
    x = maximum $ map maximum l'
    l' = map (map f) l
    offset = base - tseitinBase - 1
    f :: Int -> Int
    f x
      | abs x < tseitinBase = x
      | otherwise = signum x * (abs x + offset)

instance Ord BoolForm where
  compare (Cnf _ a) (Cnf _ b) = compare a b

compareAbsList :: [Int] -> [Int] -> Ordering
compareAbsList [] [] = EQ
compareAbsList [] _ = LT
compareAbsList _ [] = GT
compareAbsList (a:l1) (b:l2)
  | val == EQ = compareAbsList l1 l2
  | otherwise = val
  where
    val = comparing abs a b

-- | disjunction constructor
--
-- >>> let c1 = "3" -|- "4"
-- >>> asList c1
-- [[3,4,-5],[-3,5],[-4,5]]
--  
-- >>> asList (c1 -|- "-1")
-- [[3,4,-5],[-3,5],[-4,5],[5,-1,-6],[-5,6],[1,6]]
--  
-- >>> let c1 = "1" -&- "2"
-- >>> let c2 = "3" -&- "4"
-- >>> asList $ c1 -|- c2
-- [[1,3],[1,4],[2,3],[2,4]]
--  
(-|-) :: (BoolComponent a, BoolComponent b) => a -> b -> BoolForm
(toBF -> a) -|- (toBF -> b) = a -||- b

-- | conjunction constructor
--
-- >>> asList $ "3" -&- "-2"
-- [[-3,2,4],[3,-4],[-2,-4]]
--
-- >>> asList $ "3" -|- ("1" -&- "2")
-- [[-1,-2,4],[1,-4],[2,-4],[3,2,-4],[-3,4],[-2,4]]
--
(-&-) :: (BoolComponent a, BoolComponent b) => a -> b -> BoolForm
a -&- b =toBF a -&&- toBF b

-- | negate a form
-- 
-- >>> let (c1, c2, c3) = ("1" -|- "2", "2" -|- "3", "3" -|- "4" )
-- >>> asList $ neg c1
-- [[1,2,-3],[-1,3],[-2,3],[-3,-4],[3,4]]
--
-- >>> let cnf = c1 -&- (c2 -&- c3)
-- >>> asList cnf
-- [[1,2,-5],[-1,5],[-2,5],[2,3,-6],[-2,6],[-3,6],[3,4,-7],[-3,7],[-4,7],[-6,-7,8],[6,-8],[7,-8],[-5,-8,9],[5,-9],[8,-9]]
--
-- >>> asList $ neg cnf
-- [[1,2,-5],[-1,5],[-2,5],[2,3,-6],[-2,6],[-3,6],[3,4,-7],[-3,7],[-4,7],[-6,-7,8],[6,-8],[7,-8],[-5,-8,9],[5,-9],[8,-9],[-9,-10],[9,10]]
--
neg :: (BoolComponent a) => a -> BoolForm
neg (toBF -> a) = negBF a

(-!-) :: (BoolComponent a) => a -> BoolForm
(-!-) = neg

-- | implication
--
-- >>> asList ("1" ->- "2")
-- [[-1,-3],[1,3],[3,2,-4],[-3,4],[-2,4]]
--
-- >>> asList $ ("1" -&- "2") ->- ("3" -|- "4")
-- [[-1,-2,5],[1,-5],[2,-5],[-5,-6],[5,6],[3,4,-7],[-3,7],[-4,7],[6,7,-8],[-6,8],[-7,8]]
--
(->-) :: (BoolComponent a, BoolComponent b) => a -> b -> BoolForm
a ->- b = neg (toBF a) -|- toBF b

instance BoolComponent Int where
  toBF a = Cnf (a, tseitinBase) [[a]]

instance BoolComponent [Char] where
  toBF a = Cnf (v, tseitinBase) [[v]]
    where
      v = (read a)::Int

instance BoolComponent BoolForm where
  toBF = id

-- This causes 'Constraint is no smaller than the instance head'
-- So if you want to use your Enum in forms, you declase it as 'BoolComponent'
-- instance (Enum e) => BoolComponent e where
--  toBF = Lit . fromEnum

--------------------------------------------------------------------------------
-- internal functions
--------------------------------------------------------------------------------

(-||-) :: BoolForm -> BoolForm -> BoolForm
(-||-) e1 e2' =
  Cnf (m, c) $ clausesOf e1 ++ clausesOf e2 ++ [[a, b, - c], [- a, c], [- b, c]]
  where
    a = tseitinNumber e1
    e2 = renumber (a + 1) e2'
    b = tseitinNumber e2
    m = max (maxRank e1) (maxRank e2)
    c = 1 + maximum [tseitinBase, a, b]

(-&&-) :: BoolForm -> BoolForm -> BoolForm
(-&&-) e1 e2' =
  Cnf (m, c) $ clausesOf e1 ++ clausesOf e2 ++ [[- a, - b, c], [a, - c], [b, - c]]
  where
    a = tseitinNumber e1
    e2 = renumber (a + 1) e2'
    b = tseitinNumber e2
    m = max (maxRank e1) (maxRank e2)
    c = 1 + maximum [tseitinBase, a, b]

negBF :: BoolForm -> BoolForm
negBF e =
  Cnf (m, c) $ clausesOf e ++ [[- a, - c], [a, c]]
  where
    a = tseitinNumber e
    m = maxRank e
    c = 1 + maximum [tseitinBase, a]

-- | merge [BoolForm] by '(-|-)'
disjunctionOf :: [BoolForm] -> BoolForm
disjunctionOf (x:l) = foldl' (-|-) x l

-- | merge [BoolForm] by '(-&-)'
conjunctionOf :: [BoolForm] -> BoolForm
conjunctionOf (x:l) = foldr (-&-) x l

-- | converts a BoolForm to "[[Int]]"
asList :: BoolForm -> [[Int]]
asList cnf@(Cnf (m,n) _) = l'
  where
    Cnf _ l' = renumber (m + 1) cnf

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

