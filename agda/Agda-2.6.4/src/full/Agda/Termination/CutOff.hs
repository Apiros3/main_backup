{-# OPTIONS_GHC -Wunused-imports #-}

-- | Defines 'CutOff' type which is used in "Agda.Interaction.Options".
--   This module's purpose is to eliminate the dependency of
--   "Agda.TypeChecking.Monad.Base" on the termination checker and
--   everything it imports.

module Agda.Termination.CutOff
  ( CutOff(CutOff, DontCutOff)
  , defaultCutOff
  ) where

import Control.DeepSeq

-- | Cut off structural order comparison at some depth in termination checker?

data CutOff
  = CutOff !Int -- ^ @c >= 0@ means: record decrease up to including @c+1@.
  | DontCutOff
  deriving (Eq , Ord)

instance Show CutOff where
  show (CutOff k) = show k
  show DontCutOff = "∞"

instance NFData CutOff where
  rnf (CutOff _) = ()
  rnf DontCutOff = ()

-- | The default termination depth.

defaultCutOff :: CutOff
defaultCutOff = CutOff 0 -- minimum value
