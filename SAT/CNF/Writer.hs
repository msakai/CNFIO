{-# LANGUAGE Safe #-}
-- | Write SAT data to CNF file

module SAT.CNF.Writer
       (
         -- * Interface
         toFile
       )
       where
import Data.List (intercalate, nub, sort)
import System.IO

toFile :: FilePath -> [[Int]] -> IO ()
toFile f l = writeFile f $ toCNFString l
    
toCNFString :: [[Int]] -> String
toCNFString l = hdr ++ str
  where
    hdr = "p cnf " ++ show numC ++ " " ++ show numV ++ "\n"
    numC = length l
    numV = length $ nub $ sort $ map abs $ concat l
    str = intercalate "\n" [intercalate " " (map show c) ++ " 0" | c <- l]