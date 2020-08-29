module naturals where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _∎)

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

{-# BUILTIN NATURAL ℕ #-}

seven : ℕ
seven = suc (suc (suc (suc (suc (suc (suc zero))))))

four : ℕ
four = 4

_+_ : ℕ → ℕ → ℕ
zero + n = n
(suc m) + n = suc (m + n)

_ : 2 + 3 ≡ 5
_ =
  begin
    2 + 3
  ≡⟨⟩    -- is shorthand for
    (suc (suc zero)) + (suc (suc (suc zero)))
  ≡⟨⟩    -- inductive case
    suc ((suc zero) + (suc (suc (suc zero))))
  ≡⟨⟩    -- inductive case
    suc (suc (zero + (suc (suc (suc zero)))))
  ≡⟨⟩    -- base case
    suc (suc (suc (suc (suc zero))))
  ≡⟨⟩    -- is longhand for
    5
  ∎

_ : 2 + 3 ≡ 5
_ =
  begin
    2 + 3
  ≡⟨⟩
    suc (1 + 3)
  ≡⟨⟩
    suc (suc (0 + 3))
  ≡⟨⟩
    suc (suc 3)
  ≡⟨⟩
    5
  ∎

_ : 2 + 3 ≡ 5
_ = refl

_ : 3 + 4 ≡ 7
_ =
  begin
    3 + 4
  ≡⟨⟩
    suc (2 + 4)
  ≡⟨⟩
    suc (suc (1 + 4))
  ≡⟨⟩
    suc (suc (suc (0 + 4)))
  ≡⟨⟩
    suc (suc (suc 4))
  ≡⟨⟩
    7
  ∎

_*_ : ℕ → ℕ → ℕ
zero * n = zero
(suc m) * n = n + (m * n)

_ =
  begin
    2 * 3
  ≡⟨⟩    -- inductive case
    3 + (1 * 3)
  ≡⟨⟩    -- inductive case
    3 + (3 + (0 * 3))
  ≡⟨⟩    -- base case
    3 + (3 + 0)
  ≡⟨⟩    -- simplify
    6
  ∎

_ : 3 * 4 ≡ 12
_ =
  begin
    3 * 4
  ≡⟨⟩
    4 + (2 * 4)
  ≡⟨⟩
    4 + (4 + (1 * 4))
  ≡⟨⟩
    4 + (4 + (4 + (0 * 4)))
  ≡⟨⟩
    4 + (4 + (4 + 0))
  ≡⟨⟩
    12
  ∎

_^_ : ℕ → ℕ → ℕ
m ^ zero = suc zero
m ^ (suc n) = m * (m ^ n)

_ : 3 ^ 4 ≡ 81
_ =
  begin
    3 ^ 4
  ≡⟨⟩
    3 * (3 ^ 3)
  ≡⟨⟩
    3 * (3 * (3 ^ 2))
  ≡⟨⟩
    3 * (3 * (3 * (3 ^ 1)))
  ≡⟨⟩
    3 * (3 * (3 * (3 * (3 ^ 0))))
  ≡⟨⟩
    3 * (3 * (3 * (3 * 1)))
  ≡⟨⟩
    81
  ∎


_º_ : ℕ → ℕ → ℕ
m º zero = m
zero º suc n = zero
suc m º suc n = m º n

_ =
  begin
    3 º 2
  ≡⟨⟩
    2 º 1
  ≡⟨⟩
    1 º 0
  ≡⟨⟩
    1
  ∎

_ =
  begin
    2 º 3
  ≡⟨⟩
    1 º 2
  ≡⟨⟩
    0 º 1
  ≡⟨⟩
    0
  ∎

_ =
  begin
    5 º 3
  ≡⟨⟩
    4 º 2
  ≡⟨⟩
    3 º 1
  ≡⟨⟩
    2 º 0
  ≡⟨⟩
    2
  ∎


_ =
  begin
    3 º 5
  ≡⟨⟩
    2 º 4
  ≡⟨⟩
    1 º 3
  ≡⟨⟩
    0 º 2
  ≡⟨⟩
    0
  ∎

infixl 6  _+_  _º_
infixl 7  _*_

_■_ : ℕ → ℕ → ℕ
zero ■ n = n
suc m ■ n = suc (m ■ n)

{-# BUILTIN NATPLUS _+_ #-}
{-# BUILTIN NATTIMES _*_ #-}
{-# BUILTIN NATMINUS _º_ #-}

data Bin : Set where
  ⟨⟩ : Bin
  O_ : Bin → Bin
  I_ : Bin → Bin

inc : Bin → Bin
inc ⟨⟩ = O ⟨⟩
inc (O b) = I b
inc (I b) = O (inc b)

incTest : inc (I I O I ⟨⟩) ≡ (O O I I ⟨⟩)
incTest = refl

to : ℕ → Bin
to zero = O ⟨⟩
to (suc n) = inc (to n)

from : Bin → ℕ
from b = go 0 b
  where
    go : ℕ → Bin → ℕ
    go i ⟨⟩ = 0
    go i (O n) = 0 + go (i + 1) n
    go i (I n) = 2 ^ i + go (i + 1) n

testn1 : from (I (⟨⟩)) ≡ 1
testn1 = refl

testn2 : from (O (I (⟨⟩))) ≡ 2
testn2 = refl

testn3 : from (I (I (O (⟨⟩)))) ≡ 3
testn3 = refl

testn4 : from ((O (O (I (⟨⟩))))) ≡ 4
testn4 = refl

-- import Data.Nat using (ℕ; zero; suc; _+_; _*_; _^_; _∸_)
