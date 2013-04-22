{-# LANGUAGE Safe #-}

-- | Read/Write a CNF file only with ghc standard libraries
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
