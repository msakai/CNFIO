{-# LANGUAGE Safe #-}
-- | Read a CNF file without haskell-platform

module SAT.CNF
       (
         -- * Input
         fromFile
       , clauseListFromFile
         -- * Output
       , toFile
       , toCNFString
       )
       where
import SAT.CNF.Reader
import SAT.CNF.Writer
