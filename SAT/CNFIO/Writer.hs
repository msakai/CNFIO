{-# LANGUAGE Safe #-}
-- | Write SAT data to CNF file

module SAT.CNFIO.Writer
       (
         -- * Interface
         toFile
       , toCNFString
       )
       where
import Data.List (intercalate, nub, sort)
import System.IO

-- | Write the CNF to file 'f', using 'toCNFString'
toFile :: FilePath -> [[Int]] -> IO ()
toFile f l = writeFile f $ toCNFString l
    
-- | Convert [Clause] to String, where Clause is [Int]
--
-- >>> toCNFString []
-- "p cnf 0 0\n"
--
-- >>> toCNFString [[-1, 2], [-3, -4]]
-- "p cnf 4 2\n-1 2 0\n-3 -4 0\n"
--
-- >>> toCNFString [[1], [-2], [-3, -4], [1,2,3,4]]
-- "p cnf 4 4\n1 0\n-2 0\n-3 -4 0\n1 2 3 4 0\n"
--
toCNFString :: [[Int]] -> String
toCNFString l = hdr ++ str
  where
    hdr = "p cnf " ++ show numV ++ " " ++ show numC ++ "\n"
    numC = length l
    numV = last o$ nub $ sort $ map abs $ concat l
    str = concat [intercalate " " (map show c) ++ " 0\n" | c <- l]
