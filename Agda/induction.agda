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

+-assoc'' : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
+-assoc'' zero n p = refl
+-assoc'' (suc m) n p rewrite +-assoc'' m n p = refl

+-swap : ∀ (m n p : ℕ) → m + (n + p) ≡ n + (m + p)
+-swap m n p =
  begin
    m + (n + p)
  ≡⟨ sym (+-assoc m n p) ⟩
    (m + n) + p
  ≡⟨ cong (_+ p) (+-comm m n) ⟩
    (n + m) + p
  ≡⟨ +-assoc n m p ⟩
    n + (m + p)
  ∎

*-distrib-+ : ∀ (m n p : ℕ) → (m + n) * p ≡ m * p + n * p
*-distrib-+ zero n p =
  begin
    (zero + n) * p
  ≡⟨⟩
    n * p
  ≡⟨⟩
    zero * p + n * p
  ∎
*-distrib-+ (suc m) n p =
  begin
    ((suc m) + n) * p
  ≡⟨ cong (_* p) (+-comm (suc m) n) ⟩
    (n + (suc m)) * p
  ≡⟨ cong (_* p) (+-suc n m) ⟩
    (suc (n + m)) * p
  ≡⟨⟩
    p + ((n + m) * p)
  ≡⟨ cong (p +_) (*-distrib-+ n m p) ⟩
    p + (n * p + m * p)
  ≡⟨ cong (p +_) (+-comm (n * p) (m * p)) ⟩
    p + (m * p + n * p)
  ≡⟨ sym (+-assoc p (m * p) (n * p)) ⟩
    (p + (m * p)) + n * p
  ≡⟨⟩
    (suc m) * p + n * p
  ∎

*-assoc : ∀ (m n p : ℕ) → (m * n) * p ≡ m * (n * p)
*-assoc zero n p =
  begin
    (zero * n) * p
  ≡⟨⟩
    zero * p
  ≡⟨⟩
    zero
  ≡⟨⟩
    zero * (n * p)
  ∎
*-assoc (suc m) n p =
  begin
    ((suc m) * n) * p
  ≡⟨⟩
    (n + m * n) * p
  ≡⟨ *-distrib-+ n (m * n) p ⟩
    (n * p) + m * n * p
  ≡⟨ cong ((n * p) +_) (*-assoc m n p) ⟩
    (n * p) + m * (n * p)
  ≡⟨⟩
    (suc m) * (n * p)
  ∎

*-comm : ∀ (m n : ℕ) → m * n ≡ n * m
*-comm zero zero = refl
*-comm zero (suc n) =
  begin
    zero * (suc n)
  ≡⟨⟩
    zero
  ≡⟨⟩
    zero + (zero * n)
  ≡⟨ *-comm zero n ⟩
    zero + (n * zero)
  ≡⟨⟩
    (suc n) * zero
  ∎
*-comm (suc m) zero =
  begin
    (suc m) * zero
  ≡⟨⟩
    zero + (m * zero)
  ≡⟨ *-comm m zero ⟩
    (zero * m)
  ≡⟨⟩
    zero
  ≡⟨⟩
    zero * (suc m)
  ∎
*-comm (suc m) (suc n) =
  begin
    (suc m) * (suc n)
  ≡⟨⟩
    (suc n) + (m * (suc n))
  ≡⟨ cong ((suc n) +_) (*-comm m (suc n)) ⟩
    (suc n) + ((suc n) * m)
  ≡⟨⟩
    (suc n) + (m + (n * m))
  ≡⟨ +-swap (suc n) m (n * m) ⟩
    m + ((suc n) + (n * m))
  ≡⟨ sym (+-assoc m (suc n) (n * m)) ⟩
    (m + (suc n)) + (n * m)
  ≡⟨ cong (_+ (n * m)) (+-suc m n) ⟩
    suc (m + n) + (n * m)
  ≡⟨ cong suc (+-assoc m n (n * m)) ⟩
    (suc m) + (n + (n * m))
  ≡⟨ cong (λ m*n → (suc m) + (n + m*n)) (*-comm n m) ⟩
    (suc m) + (n + (m * n))
  ≡⟨⟩
    (suc m) + ((suc m) * n)
  ≡⟨ cong ((suc m) +_) (*-comm ((suc m)) n) ⟩
    (suc m) + (n * (suc m))
  ≡⟨⟩
    (suc n) * (suc m)
  ∎

0∸n≡0 : ∀ (n : ℕ) → zero ∸ n ≡ zero
0∸n≡0 zero =
  begin
    zero ∸ zero
  ≡⟨⟩
    zero
  ∎
0∸n≡0 (suc n) =
  begin
    zero ∸ (suc n)
  ≡⟨⟩
    zero
  ∎

∸-+-assoc : ∀ (m n p : ℕ) → m ∸ n ∸ p ≡ m ∸ (n + p)
∸-+-assoc m zero p =
  begin
    m ∸ zero ∸ p
  ≡⟨⟩
    m ∸ p
  ≡⟨⟩
    m ∸ (zero + p)
  ∎
∸-+-assoc m n zero =
  begin
    m ∸ n ∸ zero
  ≡⟨⟩
    m ∸ n
  ≡⟨⟩
    m ∸ (zero + n)
  ≡⟨ cong (m ∸_) (+-comm zero n) ⟩
    m ∸ (n + zero)
  ∎
∸-+-assoc zero n p =
  begin
    zero ∸ n ∸ p
  ≡⟨ cong (_∸ p) (0∸n≡0 n) ⟩
    zero ∸ p
  ≡⟨ 0∸n≡0 p ⟩
    zero
  ≡⟨ sym (0∸n≡0 (n + p)) ⟩
    zero ∸ (n + p)
  ∎
∸-+-assoc (suc m) (suc n) p =
  begin
    (suc m) ∸ (suc n) ∸ p
  ≡⟨⟩
    m ∸ n ∸ p
  ≡⟨ ∸-+-assoc m n p ⟩
    m ∸ (n + p)
  ≡⟨⟩
    (suc m) ∸ ((suc n) + p)
  ∎
