{-# LANGUAGE Safe #-}

-- | Read/Write a CNF file only with ghc standard libraries
module SAT.CNFIO
       (
         -- * Input
         fromFile
       , clauseListFromFile
       , fromMinisatOutput
       , clauseListFromMinisatOutput
         -- * Output
       , toFile
       , toCNFString
       , asCNFString
       , asCNFString_
         -- * Bool Operation
       , module SAT.BoolExp
       )
       where
import SAT.CNFIO.Reader
import SAT.CNFIO.Writer
import SAT.CNFIO.MinisatReader
import SAT.BoolExp

-- | String from BoolFrom
asCNFString :: BoolForm -> String
asCNFString = toCNFString . asList

-- | String from BoolFrom
asCNFString_ :: BoolForm -> String
asCNFString_ = toCNFString . asList_
