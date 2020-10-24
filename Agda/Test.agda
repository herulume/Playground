module Test where

open import Data.Bool
open import Function
open import Relation.Binary.PropositionalEquality

open ≡-Reasoning

-- Idempotent
Idempotent : ∀ {a} → (a → a) → Set
Idempotent f = ∀ z → f (f z) ≡ f z

andIdempotent : ∀ b → Idempotent (_∧_ b)
andIdempotent false z = refl
andIdempotent true z = refl

idIdempotent : ∀ a → Idempotent (id {A = a})
idIdempotent _ _ = refl

-- Involution
Involution : ∀ {a} → (a → a) → Set
Involution f = ∀ z → f (f z) ≡ z

notInvolution : Involution not
notInvolution false = refl
notInvolution true = refl

xorInvolution : ∀ b → Involution (_xor_ b)
xorInvolution false z = refl
xorInvolution true false = refl
xorInvolution true true = refl

-- monoids and oh my
record Semigroup (a : Set) : Set where
  field
    _⊛_ : a → a → a
    associative : ∀ a b c → (a ⊛ b) ⊛ c ≡ a ⊛ (b ⊛ c)


andAssociative : ∀ a b c → (a ∧ b) ∧ c ≡ a ∧ (b ∧ c)
andAssociative false b c = refl
andAssociative true b c = refl

andSemigroup : Semigroup Bool
andSemigroup = record
  { _⊛_ = _∧_
  ; associative = andAssociative
  }

orAssociative : ∀ a b c → (a ∨ b) ∨ c ≡ a ∨ (b ∨ c)
orAssociative false b c = refl
orAssociative true b c = refl

orSemigroup : Semigroup Bool
orSemigroup = record
  { _⊛_ = _∨_
  ; associative = orAssociative
  }


record Monoid (a : Set) : Set where
  field
    semigroup : Semigroup a
  open Semigroup semigroup
  field
    empty : a
    leftIdentity : ∀ a → empty ⊛ a ≡ a
    rightIdentity : ∀ a → a ⊛ empty  ≡ a

andRightIdentity : ∀ a → a ∧ true ≡ a
andRightIdentity false = refl
andRightIdentity true = refl

andMonoid : Monoid Bool
andMonoid = record
  { semigroup = andSemigroup
  ; empty = true
  ; leftIdentity = λ _ → refl
  ; rightIdentity = andRightIdentity
  }

-- These monad
data These a b : Set where
  This : a → These a b
  That : b → These a b
  Both : a → b → These a b

flatMapThese :
  ∀ {s a b} →
  Semigroup s →
  These s a →
  (a → These s b) →
  These s b
flatMapThese _ (This x1) _ = This x1
flatMapThese _ (That x1) x2 = x2 x1
flatMapThese _ (Both _ x2) x3 with x3 x2
flatMapThese s (Both x1 _) _ | This x₁ =
  This (x1 ⊛ x₁)
  where open Semigroup s
flatMapThese s (Both x1 _) _ | That x₁ = Both x1 x₁
flatMapThese s (Both x1 _) _ | Both x₁ x₂ =
  Both (x1 ⊛ x₁) x₂
  where open Semigroup s

record Bind (m : Set → Set) : Set₁ where
  field
    _>>=_ : ∀ {a b} → m a → (a → m b) → m b
    associative : ∀ {a b c} (x : m a) (f : a → m b) (g : b → m c) → (x >>= f) >>= g ≡ x >>= (λ a → f a >>= g)

associativeThese :
  ∀ {a b c l} →
  (s : Semigroup l) →
  (x : These l a)
  (f : a → These l b)
  (g : b → These l c) →
  flatMapThese s (flatMapThese s x f) g ≡
    flatMapThese s x (λ a → flatMapThese s (f a) g)
associativeThese s (This x) f g = refl
associativeThese s (That x) f g = refl
associativeThese s (Both x x₁) f g with f x₁
associativeThese s (Both x x₁) f g | This x₂ = refl
associativeThese s (Both x x₁) f g | That x₂ with g x₂
associativeThese s (Both x x₁) f g | That x₂ | This x₃ = refl
associativeThese s (Both x x₁) f g | That x₂ | That x₃ = refl
associativeThese s (Both x x₁) f g | That x₂ | Both x₃ x₄ = refl
associativeThese s (Both x x₁) f g | Both x₂ x₃ with g x₃
associativeThese s (Both x x₁) f g | Both x₂ x₃ | This x₄ =
  begin
  This ((x ⊛ x₂) ⊛ x₄) ≡⟨ cong This (Semigroup.associative s x x₂ x₄) ⟩
  This (x ⊛ (x₂ ⊛ x₄))
  ∎
  where open Semigroup s
associativeThese s (Both x x₁) f g | Both x₂ x₃ | That x₄ = refl
associativeThese s (Both x x₁) f g | Both x₂ x₃ | Both x₄ x₅ =
  begin
  Both ((x ⊛ x₂) ⊛ x₄) x₅ ≡⟨ cong (flip Both x₅)  (Semigroup.associative s x x₂ x₄) ⟩
  Both (x ⊛ (x₂ ⊛ x₄)) x₅
  ∎
  where open Semigroup s


bindThese : ∀ {s} → Semigroup s → Bind (These s)
bindThese s  = record
  { _>>=_ = flatMapThese s
  ; associative = associativeThese s
  }

record Monad m : Set₁ where
  field
    bind : Bind m
  open Bind bind
  field
    return : ∀ {a} (x : a) → m a
    leftIdentity : ∀ {a b} x (f : a → m b) → return x >>= f ≡ f x
    rightIdentity : ∀ {a} (x : m a) → x >>= return ≡ x

theseRightIdentity :
  ∀ {a l}
  (s : Semigroup l)
  (x : These l a) →
  flatMapThese s x That ≡ x
theseRightIdentity s (This x) = refl
theseRightIdentity s (That x) = refl
theseRightIdentity s (Both x x₁) = refl

monadThese : ∀ {s} → Semigroup s → Monad (These s)
monadThese s = record
  { bind = bindThese s
  ; return = That
  ; leftIdentity = λ _ _ → refl
  ; rightIdentity = theseRightIdentity s
  }
