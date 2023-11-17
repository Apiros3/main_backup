import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; sym)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; step-≡; _∎)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _∸_;_^_)

module test where

+-assoc : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
+-assoc zero n p =
  begin
    (zero + n) + p
  ≡⟨⟩
    n + p
  ≡⟨⟩
    zero + (n + p)
  ∎
+-assoc (suc m) n p =
  begin
    (suc m + n) + p
  ≡⟨⟩
    suc (m + n) + p
  ≡⟨⟩
    suc ((m + n) + p)
  ≡⟨ cong suc (+-assoc m n p) ⟩
    suc (m + (n + p))
  ≡⟨⟩
    suc m + (n + p)
  ∎

+-identity : ∀ (m : ℕ) → m + zero ≡ m
+-identity zero = 
    begin
        zero + zero 
    ≡⟨⟩
        zero 
    ∎
+-identity (suc m) = 
    begin
        suc m + zero 
    ≡⟨⟩
        suc (m + zero)
    ≡⟨ cong suc (+-identity m) ⟩
        suc (m)
    ∎

+-identityl : ∀ (m : ℕ) → zero + m ≡ m
+-identityl zero = 
    begin
        zero + zero 
    ≡⟨⟩
        zero 
    ∎
+-identityl (suc m) = 
    begin
        zero + suc m 
    ≡⟨⟩
        suc m
    ∎



