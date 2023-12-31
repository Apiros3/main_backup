------------------------------------------------------------------------
-- The Agda standard library
--
-- A categorical view of Colist
------------------------------------------------------------------------

{-# OPTIONS --cubical-compatible --sized-types #-}

module Codata.Colist.Categorical where

open import Codata.Conat using (infinity)
open import Codata.Colist
open import Category.Functor
open import Category.Applicative

functor : ∀ {ℓ i} → RawFunctor {ℓ} (λ A → Colist A i)
functor = record { _<$>_ = map }

applicative : ∀ {ℓ i} → RawApplicative {ℓ} (λ A → Colist A i)
applicative = record
  { pure = replicate infinity
  ; _⊛_  = ap
  }
