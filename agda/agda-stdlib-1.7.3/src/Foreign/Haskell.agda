------------------------------------------------------------------------
-- The Agda standard library
--
-- Type(s) used (only) when calling out to Haskell via the FFI
------------------------------------------------------------------------

{-# OPTIONS --cubical-compatible #-}

module Foreign.Haskell where

open import Level

------------------------------------------------------------------------
-- Pairs

open import Foreign.Haskell.Pair public
  renaming
  ( toForeign   to toForeignPair
  ; fromForeign to fromForeignPair
  )

------------------------------------------------------------------------
-- Sums

open import Foreign.Haskell.Either public
  renaming
  ( toForeign   to toForeignEither
  ; fromForeign to fromForeignEither
  )

------------------------------------------------------------------------
-- DEPRECATED NAMES
------------------------------------------------------------------------
-- Please use the new names as continuing support for the old names is
-- not guaranteed.

open import Data.Unit using (⊤; tt)

-- Version 1.1

Unit = ⊤
{-# WARNING_ON_USAGE Unit
"Warning: Unit was deprecated in v1.1.
Please use ⊤ from Data.Unit instead."
#-}
unit = tt
{-# WARNING_ON_USAGE unit
"Warning: unit was deprecated in v1.1.
Please use tt from Data.Unit instead."
#-}
