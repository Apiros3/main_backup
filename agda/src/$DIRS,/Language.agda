
open import Data.List
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; sym; subst)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; step-≡; _∎)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _∸_;_^_)
open import Data.Fin 
open import Data.Fin.Properties
open import Data.Fin.Subset
open import Data.Vec hiding (_++_)
-- open import Data.Bool 


module Language where 

module _ {Σ : Set} where
    Word = List Σ

    -- ε : Word 
    pattern ε = [] 

    _∙_ : Word → Word → Word 
    u ∙ v = u ++ v 

    ε-runit : (w : Word) → w ∙ ε ≡ w 
    ε-runit ε = refl
    ε-runit (x ∷ w) = cong (x ∷_) (ε-runit w)

    ε-lunit : (w : Word) → ε ∙ w ≡ w 
    ε-lunit ε = refl
    ε-lunit (x ∷ w) = begin 
            ε ∙ (x ∷ w)
        ≡⟨⟩ 
            [] ++ (x ∷ w)
        ≡⟨⟩ 
            (x ∷ w)
        ∎

    ∙assoc : ∀ (m n p : Word) → (m ∙ n) ∙ p ≡ m ∙ (n ∙ p) 
    ∙assoc ε n p = refl
    ∙assoc (x ∷ m) n p = cong (x ∷_) (∙assoc m n p)  
    
    record DFA (n : ℕ) : Set where 
        Q = Fin n
        field 
            -- Q : Fin n 
            δ : Q → Σ → Q  
            F : Subset n 
            q₀ : Q 
    open DFA 

    δ^ : ∀ {n} (dfa : DFA n) → (q : Fin n) → Word → Fin n  
    δ^ dfa q ε = q 
    δ^ dfa q (x ∷ xs) = δ^ dfa (δ dfa q x) xs  

    accept : ∀ {n} -> (dfa : DFA n) → Word → Set
    accept {n} dfa x =  (δ^ dfa q₀ x) ∈ F 
        -- where 
        -- open DFA dfa 
        -- e : Fin n 
        -- e = δ^ dfa q₀ x
     
    -- L : ∀ {n} → (dfa : DFA n) → 
    
open DFA

w : Word
w = 0 ∷ 1 ∷ 0 ∷ []

emptydfa : DFA {Σ = ℕ} 1 
emptydfa = record { δ = λ x x₁ → zero ; F = outside ∷ [] ; q₀ = zero } 

test : Fin 1
test = δ^ emptydfa zero w 