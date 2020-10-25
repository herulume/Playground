module These where

open import Function
open import Relation.Binary.PropositionalEquality
open ≡-Reasoning

-- Some definitions
record Semigroup (a : Set) : Set where
  field
    _⊛_ : a → a → a
    associative : ∀ a b c → (a ⊛ b) ⊛ c ≡ a ⊛ (b ⊛ c)

record Bind (m : Set → Set) : Set₁ where
  field
    _>>=_ : ∀ {a b} → m a → (a → m b) → m b
    associative : ∀ {a b c} (x : m a) (f : a → m b) (g : b → m c) → (x >>= f) >>= g ≡ x >>= (λ a → f a >>= g)

record Monad m : Set₁ where
  field
    bind : Bind m
  open Bind bind
  field
    return : ∀ {a} (x : a) → m a
    leftIdentity : ∀ {a b} x (f : a → m b) → return x >>= f ≡ f x
    rightIdentity : ∀ {a} (x : m a) → x >>= return ≡ x

-- Our data type
data These a b : Set where
  This : a → These a b
  That : b → These a b
  Both : a → b → These a b

-- >>=
flatMapThese : ∀ {s a b} → Semigroup s → These s a → (a → These s b) → These s b
flatMapThese _ (This x₁) _ = This x₁
flatMapThese _ (That x₁) f = f x₁
flatMapThese _ (Both _ r) f with f r
flatMapThese s (Both l _) _ | This x₁ = This (l ⊛ x₁) where open Semigroup s
flatMapThese _ (Both l _) _ | That x₁ = Both l x₁
flatMapThese s (Both l _) _ | Both l₁ r₁ = Both (l ⊛ l₁) r₁ where open Semigroup s

-- auxiliary proofs
associativeThese : ∀ {a b c l} (s : Semigroup l) (x : These l a) (f : a → These l b) (g : b → These l c) → flatMapThese s (flatMapThese s x f) g ≡ flatMapThese s x (λ a → flatMapThese s (f a) g)
associativeThese s (This x) f g = refl
associativeThese s (That x) f g = refl
associativeThese s (Both x x₁) f g with f x₁
associativeThese s (Both x x₁) f g | This x₂ = refl
associativeThese s (Both x x₁) f g | That x₂ with g x₂
associativeThese s (Both x x₁) f g | That x₂ | This x₃ = refl
associativeThese s (Both x x₁) f g | That x₂ | That x₃ = refl
associativeThese s (Both x x₁) f g | That x₂ | Both x₃ x₄ = refl
associativeThese s (Both x x₁) f g | Both x₂ x₃ with g x₃
associativeThese s (Both x x₁) f g | Both x₂ x₃ | This x₄ = cong This (Semigroup.associative s x x₂ x₄) where open Semigroup s
associativeThese s (Both x x₁) f g | Both x₂ x₃ | That x₄ = refl
associativeThese s (Both x x₁) f g | Both x₂ x₃ | Both x₄ x₅ = cong (flip Both x₅) (Semigroup.associative s x x₂ x₄) where open Semigroup s

theseRightIdentity : ∀ {a l} (s : Semigroup l) (x : These l a) → flatMapThese s x That ≡ x
theseRightIdentity s (This x) = refl
theseRightIdentity s (That x) = refl
theseRightIdentity s (Both x x₁) = refl

-- yay it's a bind...
bindThese : ∀ {s} → Semigroup s → Bind (These s)
bindThese s  = record
  { _>>=_ = flatMapThese s
  ; associative = associativeThese s
  }

-- AND A MONAD
monadThese : ∀ {s} → Semigroup s → Monad (These s)
monadThese s = record
  { bind = bindThese s
  ; return = That
  ; leftIdentity = λ _ _ → refl
  ; rightIdentity = theseRightIdentity s
  }
