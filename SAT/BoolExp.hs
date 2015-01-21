{-# LANGUAGE BangPatterns, FlexibleInstances, ViewPatterns, UndecidableInstances #-}
-- | Boolean Expression module to build CNF from arbitrary expressions
-- Tseitin translation: http://en.wikipedia.org/wiki/Tseitin_transformation
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

import Data.List (foldl', intercalate)

tseitinBase :: Int
tseitinBase = 800000

class BoolComponent a where
  toBF :: a -> BoolForm   -- lift to BoolForm

data BoolForm = Cnf (Int, Int) [[Int]]
    deriving (Eq, Show)

instance BoolComponent Int where
  toBF a = Cnf (abs a, max tseitinBase (abs a)) [[a]]

instance BoolComponent [Char] where
  toBF (read -> a) = Cnf (abs a, max tseitinBase (abs a)) [[a]]

instance BoolComponent BoolForm where
  toBF = id

-- | return a 'clause' list only if it contains some real clause (not a literal) 
clausesOf :: BoolForm -> [[Int]]
clausesOf cnf@(Cnf _ [[]]) = []
clausesOf cnf@(Cnf _ [[x]]) = []
clausesOf cnf@(Cnf _ l) = l

maxRank :: BoolForm -> Int
maxRank (Cnf (n, _) _) = n

-- | この部分式の出力を表す変数を返す。
-- ただし単一リテラルの場合はその値を返す。
-- 単一リテラルの場合は負数を返す場合がある。
-- それ以外の場合は 'tseitinBase' よりも大きな正数である。
-- 従って`1+ max tseitinBase 返値`は常にこの式に含まれない次の一時変数を示す。
tseitinNumber :: BoolForm -> Int
tseitinNumber (Cnf (m, n) [[x]]) = x
tseitinNumber (Cnf (_, n) _) = n

renumber :: Int -> BoolForm -> (BoolForm, Int)
renumber base (Cnf (m, n) l) = (Cnf (m, n') l', n')
  where
    l' = map (map f) l
    n' = maximum $ map maximum l'
    offset = base - tseitinBase - 1
    f x = if abs x < tseitinBase then x else signum x * (abs x + offset)

instance Ord BoolForm where
  compare (Cnf _ a) (Cnf _ b) = compare a b

-- | disjunction constructor
--
-- >>> asList $ "3" -|- "4"
-- [[3,4,-5],[-3,5],[-4,5]]
--  
-- >>> asList (("3" -|- "4") -|- "-1")
-- [[3,4,-5],[-3,5],[-4,5],[5,-1,-6],[-5,6],[1,6]]
--
(-|-) :: (BoolComponent a, BoolComponent b) => a -> b -> BoolForm
(toBF -> e1) -|- (toBF -> e2') = 
  Cnf (m, c) $ clausesOf e1 ++ clausesOf e2 ++ [[a, b, - c], [- a, c], [- b, c]]
  where
    a = tseitinNumber e1
    (e2, b) = renumber (1 + max tseitinBase a) e2'
    m = max (maxRank e1) (maxRank e2)
    c = 1 + max tseitinBase (max a b)

-- | conjunction constructor
--
-- >>> asList $ "3" -&- "-2"
-- [[-3,2,4],[3,-4],[-2,-4]]
--
-- >>> asList $ "3" -|- ("1" -&- "2")
-- [[-1,-2,4],[1,-4],[2,-4],[3,4,-5],[-3,5],[-4,5]]
--
(-&-) :: (BoolComponent a, BoolComponent b) => a -> b -> BoolForm
(toBF -> e1) -&- (toBF -> e2') =
  Cnf (m, c) $ clausesOf e1 ++ clausesOf e2 ++ [[- a, - b, c], [a, - c], [b, - c]]
  where
    a = tseitinNumber e1
    (e2, b) = renumber (1 + max tseitinBase a) e2'
    m = max (maxRank e1) (maxRank e2)
    c = 1 + max tseitinBase (max a b)

-- | negate a form
-- 
-- >>> asList $ neg ("1" -|- "2")
-- [[1,2,-3],[-1,3],[-2,3],[-3,-4],[3,4]]
neg :: (BoolComponent a) => a -> BoolForm
neg (toBF -> e) =
  Cnf (m, c) $ clausesOf e ++ [[- a, - c], [a, c]]
  where
    a = tseitinNumber e
    m = maxRank e
    c = 1 + max tseitinBase a

(-!-) :: (BoolComponent a) => a -> BoolForm
(-!-) = neg

-- | implication as a short cut
--
-- >>> asList ("1" ->- "2")
-- [[-1,-3],[1,3],[3,2,-4],[-3,4],[-2,4]]
(->-) :: (BoolComponent a, BoolComponent b) => a -> b -> BoolForm
(toBF -> a) ->- (toBF -> b) = (neg a) -|- b

-- | merge [BoolForm] by '(-|-)'
disjunctionOf :: [BoolForm] -> BoolForm
disjunctionOf [] = Cnf (0, tseitinBase) [[]]
disjunctionOf (x:l) = foldl' (-|-) x l

-- | merge [BoolForm] by '(-&-)'
conjunctionOf :: [BoolForm] -> BoolForm
conjunctionOf [] = Cnf (0, tseitinBase) [[]]
conjunctionOf (x:l) = foldl' (-&-) x l

-- | converts a BoolForm to "[[Int]]"
asList :: BoolForm -> [[Int]]
asList cnf@(Cnf (m,_) _) = l'
  where
    (Cnf _ l', _) = renumber (m + 1) cnf

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
