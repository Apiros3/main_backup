------------------------------------------------------------------------
-- The Agda standard library
--
-- Floating point numbers
------------------------------------------------------------------------

{-# OPTIONS --cubical-compatible --safe #-}

module Data.Float where

------------------------------------------------------------------------
-- Re-export base definitions and decidability of equality

open import Data.Float.Base public
open import Data.Float.Properties using (_≈?_; _≟_) public
