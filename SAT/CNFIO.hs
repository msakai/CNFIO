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
         -- * Bool Operation
       , BoolComponent (..)
       , BoolForm (..)
       , neg
       , (-!-)
       , (-|-)
       , (-&-)
       , (->-)
       , asList
       , asLatex
       , asCNFString
       )
       where
import SAT.CNFIO.Reader
import SAT.CNFIO.Writer
import SAT.CNFIO.MinisatReader
import SAT.BoolExp


asCNFString :: BoolForm -> String
asCNFString = toCNFString . asList
