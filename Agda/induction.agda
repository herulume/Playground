module induction where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; sym)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; step-≡; _∎)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _∸_)

_ : (3 + 4) + 5 ≡ 3 + (4 + 5)
_ =
  begin
    (3 + 4) + 5
  ≡⟨⟩
    7 + 5
  ≡⟨⟩
    12
  ≡⟨⟩
    3 + 9
  ≡⟨⟩
    3 + (4 + 5)
  ∎

+-assoc : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
+-assoc zero n p =
  begin
    (zero + n) + p
  ≡⟨⟩
    (n + p)
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
  ≡⟨ cong suc (+-assoc m n p) ⟩ -- ≡ congruente face a suc
    suc (m + (n + p))
  ≡⟨⟩
    suc m + (n + p)
  ∎

+-assoc-2 : ∀ (n p : ℕ) → (2 + n) + p ≡ 2 + (n + p)
+-assoc-2 n p =
  begin
    (2 + n) + p
  ≡⟨⟩
    suc (1 + n) + p
  ≡⟨⟩
    suc ((1 + n) + p)
  ≡⟨ cong suc (+-assoc-1 n p) ⟩
    suc (1 + (n + p))
  ≡⟨⟩
    2 + (n + p)
  ∎
  where
  +-assoc-1 : ∀ (n p : ℕ) → (1 + n) + p ≡ 1 + (n + p)
  +-assoc-1 n p =
    begin
      (1 + n) + p
    ≡⟨⟩
      suc (0 + n) + p
    ≡⟨⟩
      suc ((0 + n) + p)
    ≡⟨ cong suc (+-assoc-0 n p) ⟩
      suc (0 + (n + p))
    ≡⟨⟩
      1 + (n + p)
    ∎
    where
    +-assoc-0 : ∀ (n p : ℕ) → (0 + n) + p ≡ 0 + (n + p)
    +-assoc-0 n p =
      begin
        (0 + n) + p
      ≡⟨⟩
        n + p
      ≡⟨⟩
        0 + (n + p)
      ∎

+-identityʳ : ∀ (m : ℕ) → m + zero ≡ m
+-identityʳ zero =
  begin
    zero + zero
  ≡⟨⟩
    zero
  ∎
+-identityʳ (suc m) =
  begin
    suc m + zero
  ≡⟨⟩
    suc (m + zero)
  ≡⟨ cong suc (+-identityʳ m) ⟩
    suc m
  ∎

+-suc : ∀ (m n : ℕ) → m + suc n ≡ suc (m + n)
+-suc zero n =
  begin
    zero + suc n
  ≡⟨⟩
    suc n
  ≡⟨⟩
    suc (zero + n)
  ∎
+-suc (suc m) n =
  begin
    suc m + suc n
  ≡⟨⟩
    suc (m + suc n)
  ≡⟨ cong suc (+-suc m n) ⟩
    suc (suc (m + n))
  ≡⟨⟩
    suc (suc m + n)
  ∎

+-comm : ∀ (m n : ℕ) → m + n ≡ n + m
+-comm m zero =
  begin
    m + zero
  ≡⟨ +-identityʳ m ⟩
    m
  ≡⟨⟩
    zero + m
  ∎
+-comm m (suc n) =
  begin
    m + suc n
  ≡⟨ +-suc m n ⟩
    suc (m + n)
  ≡⟨ cong suc (+-comm m n) ⟩
    suc (n + m)
  ≡⟨⟩
    suc n + m
  ∎

+-rearrange : ∀ (m n p q : ℕ) → (m + n) + (p + q) ≡ m + (n + p) + q
+-rearrange m n p q =
  begin
    (m + n) + (p + q)
  ≡⟨ +-assoc m n (p + q) ⟩
    m + (n + (p + q))
  ≡⟨ cong (m +_) (sym (+-assoc n p q)) ⟩
    m + ((n + p) + q)
  ≡⟨ sym (+-assoc m (n + p) q) ⟩
    (m + (n + p)) + q
  ∎

-- Exercise finite-+-assoc
-- Day 0

-- Day 1
-- 0 : ℕ

-- Day 2
-- 1 : ℕ
-- (0 + 0) + 0 ≡ 0 + (0 + 0)

-- Day 3
-- 2 : ℕ
-- (0 + 0) + 1 ≡ 0 + (0 + 1)
-- (0 + 1) + 0 ≡ 0 + (1 + 0)
-- (0 + 1) + 1 ≡ 0 + (1 + 1)
-- (1 + 0) + 0 ≡ 1 + (0 + 0)
-- (1 + 0) + 1 ≡ 1 + (0 + 1)
-- (1 + 1) + 0 ≡ 1 + (1 + 0)
-- (1 + 1) + 1 ≡ 1 + (1 + 1)

-- Day 4
-- 3 : ℕ
-- (0 + 0) + 2 ≡ 0 + (0 + 2)
-- (0 + 1) + 2 ≡ 0 + (1 + 2)
-- (0 + 2) + 0 ≡ 0 + (2 + 0)
-- (0 + 2) + 1 ≡ 0 + (2 + 1)
-- (0 + 2) + 2 ≡ 0 + (2 + 2)
-- (1 + 0) + 2 ≡ 1 + (0 + 2)
-- (1 + 1) + 2 ≡ 1 + (1 + 2)
-- (1 + 2) + 0 ≡ 1 + (2 + 0)
-- (1 + 2) + 1 ≡ 1 + (2 + 1)
-- (1 + 2) + 2 ≡ 1 + (2 + 2)
-- (2 + 0) + 0 ≡ 2 + (0 + 0)
-- (2 + 0) + 1 ≡ 2 + (0 + 1)
-- (2 + 0) + 2 ≡ 2 + (0 + 2)
-- (2 + 1) + 0 ≡ 2 + (1 + 0)
-- (2 + 1) + 1 ≡ 2 + (1 + 1)
-- (2 + 1) + 2 ≡ 2 + (1 + 2)
-- (2 + 2) + 0 ≡ 2 + (2 + 0)
-- (2 + 2) + 1 ≡ 2 + (2 + 1)
-- (2 + 2) + 2 ≡ 2 + (2 + 2)

+-assoc′ : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
+-assoc′ zero    n p                         = refl
+-assoc′ (suc m) n p  rewrite +-assoc′ m n p = refl

+-identity′ : ∀ (n : ℕ) → n + zero ≡ n
+-identity′ zero = refl
+-identity′ (suc n) rewrite +-identity′ n = refl

+-suc′ : ∀ (m n : ℕ) → m + suc n ≡ suc (m + n)
+-suc′ zero n = refl
+-suc′ (suc m) n rewrite +-suc′ m n = refl

+-comm′ : ∀ (m n : ℕ) → m + n ≡ n + m
+-comm′ m zero rewrite +-identity′ m = refl
+-comm′ m (suc n) rewrite +-suc′ m n | +-comm′ m n = refl
