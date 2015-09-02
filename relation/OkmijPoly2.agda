-- Typeclass/inductive relation with backtracking
-- inspired by
--
-- Oleg Kiselyov's
-- Type-class overloaded functions:
-- Second-order typeclass programming with backtracking
-- http://okmij.org/ftp/Haskell/poly2.txt
--
-- Usage scenario:
--
-- Overload the operator * so that it multiplies
-- matrices with matrices, vectors with matrices,
-- and matrices with vectors of correct dimensions
-- and permits nested matrices.

module OkmijPoly2 where

import Level
open import Function
open import Category.Monad
open import Data.List hiding (sum)
open RawMonad (monad {Level.zero}) using (_>>=_)
open import Data.Unit hiding (_≟_)
open import Data.Empty
open import Data.Product hiding (∃ ; map)
open import Relation.Binary.PropositionalEquality
open import Relation.Nullary.Core

import Data.Float    as Float
import Data.Rational as Rational
import Data.Integer  as Integer
import Data.Nat      as Nat
import Data.Bool     as Bool
import Data.Char     as Char
import Data.String   as String
import Data.Vec      as Vec

open Float    using (Float)
open Rational using (ℚ)
open Integer  using (ℤ)
open Nat      using (ℕ ; _≟_)
open Bool     using (Bool ; true ; false)
open Char     using (Char)
open String   using (String)
open Vec      using (Vec ; [] ; _∷_)

postulate error : ∀ {A : Set} → A

data Type : Set → Set₁ where
  flo : Type Float
  rat : Type ℚ
  int : Type ℤ
  nat : Type ℕ
  cha : Type Char
  str : Type String
  boo : Type Bool
  vec : ∀ {A} → Type A → (n : ℕ) → Type (Vec A n)

Matrix : (A : Set) (m n : ℕ) → Set
Matrix A m n = Vec (Vec A n) m

pattern vector A n = vec {A} _ n
pattern matrix A m n = vec (vec {A} _ n ) m

instance floa : Type Float  ; floa = flo
instance rati : Type ℚ      ; rati = rat
instance inte : Type ℤ      ; inte = int
instance natu : Type ℕ      ; natu = nat
instance char : Type Char   ; char = cha
instance stri : Type String ; stri = str
instance bool : Type Bool   ; bool = boo

instance vect : ∀ {A n} {{_ : Type A}} → Type (Vec A n)
vect {{x}} = vec x _

instance tttt : ⊤ ; tttt = tt

Nil : (T : Set) {{_ : Type T}} → List T
Nil ._ {{nat}} = 0 ∷ []
Nil  + {{ _ }} = []

infixr 1 ∃_

∃_ : ∀ {T : Set} → List T → Set
∃ _ ∷ [] = ⊤
∃ _      = ⊥

witness : ∀ {T : Set} (x : List T) {{witness : ∃ x}} → T
witness (x ∷ []) {{tt}} = x
witness [] {{()}}
witness (_ ∷ _ ∷ _) {{()}}

nil : (T : Set) {{_ : Type T}} {{witness : ∃ Nil T}} → T
nil T = witness (Nil T)

nil-is-good : nil ℕ ≡ 0
nil-is-good = refl

Add : (A B C : Set) {{_ : Type A}} {{_ : Type B}} {{_ : Type C}} →
      List (A → B → C)
Add ._ ._ ._ {{nat}} {{nat}} {{nat}} = Nat._+_ ∷ []
Add  _  _  _ {{ _ }} {{ _ }} {{ _ }} = []

_+_ : {A B C : Set} {{_ : Type A}} {{_ : Type B}} {{_ : Type C}} →
      {{witness : ∃ Add A B C}} → A → B → C
_+_ = witness (Add _ _ _)

add-is-good : 2 + 2 ≡ 4
add-is-good = refl

Sum : (T : Set) {{_ : Type T}} → List (∀ {n} → Vec T n → T)
Sum T {{_}} with Nil T | Add T T T
... | nil' ∷ [] | add' ∷ [] = Vec.foldr (const T) add' nil' ∷ []
... | ========= | -=======- = []

sum : {T : Set} {{_ : Type T}} {{witness : ∃ Sum T}} → ∀ {n} → Vec T n → T
sum = witness (Sum _)

sum-is-good : let open Vec hiding (sum) ; open List using () in
              sum (1 ∷ 2 ∷ 3 ∷ 4 ∷ []) ≡ 10
sum-is-good = refl

fill : (n : ℕ) → {A : Set} → A → Vec A n
fill Nat.zero x = []
fill (Nat.suc n) x = x ∷ fill n x

_<*>_ : ∀ {A B : Set} {n} → Vec (A → B) n → Vec A n → Vec B n
Vec.[] <*> Vec.[] = []
(Vec._∷_ f fs) <*> (Vec._∷_ x xs) = Vec._∷_ (f x) (fs <*> xs)

transpose : ∀ {A m n} → Matrix A m n → Matrix A n m
transpose [] = fill _ []
transpose (row ∷ rest) = Vec.map _∷_ row <*> transpose rest

-- trust me, this is terminating
{-# NO_TERMINATION_CHECK #-}
Mul Mul2 Mul3 Mul4 Mul5 Mul6 :
  (A B C : Set) {{_ : Type A}} {{_ : Type B}} {{_ : Type C}} → List (A → B → C)

Mul ._ ._ ._ {{nat}} {{nat}} {{nat}} = Nat._*_ ∷ Mul2 _ _ _
Mul _ _ _ {{_}} {{_}} {{_}} = Mul2 _ _ _

Mul2 ._ ._ ._ {{matrix A m n}} {{matrix B n' k}} {{matrix C m' k'}}
  with m ≟ m' | n ≟ n' | k ≟ k'
... | yes m=m' | yes n=n' | yes k=k' = helper m m' n n' k k' m=m' n=n' k=k'
  where
    helper : (m m' n n' k k' : ℕ) →
             m ≡ m' → n ≡ n' → k ≡ k' →
             List (Matrix A m n → Matrix B n' k → Matrix C m' k')
    helper m .m n .n k .k refl refl refl
      with Sum C | Mul (Vec A n) (Vec B n) C
    ... | sums | muls = (sums >>= (λ sum' → flip map muls (λ mul' →
      (λ M N → Vec.map (λ row → Vec.map (λ col → mul' row col) (transpose N)) M)
      ))) ++ Mul3 _ _ _

... | _ | _ | _ = Mul3 _ _ _
Mul2 _ _ _ {{_}} {{_}} {{_}} = Mul3 _ _ _


-- vector * vector = scalar
Mul3 ._ ._ C {{vector A n}} {{vector B n'}} {{_}} with n ≟ n'
... | yes n=n' = helper n n' n=n' where
  helper : (n n' : ℕ) → n ≡ n' → List (Vec A n → Vec B n' → C)
  helper n .n refl with Sum C | Mul A B C
  ... | sums | muls = (sums >>= (λ sum' → flip map muls (λ mul' →
    λ xs ys → sum' (Vec.zipWith mul' xs ys)
    ))) ++ Mul4 _ _ _

... | _ = Mul4 _ _ _
Mul3 _ _ _ {{_}} {{_}} {{_}} = Mul4 _ _ _

-- matrix * vector = vector
Mul4 ._ ._ ._ {{matrix A m n}} {{vector B n'}} {{vector C m'}}
  with m ≟ m' | n ≟ n'
... | yes m=m' | yes n=n' = helper m m' n n' m=m' n=n' where
  helper : (m m' n n' : ℕ) → m ≡ m' → n ≡ n' →
           List (Matrix A m n → Vec B n' → Vec C m')
  helper m .m n .n refl refl with Sum C | Mul (Vec A n) (Vec B n) C
  ... | sums | muls = (sums >>= (λ sum' → flip map muls (λ mul' →
    λ xss ys → Vec.map (λ xs → mul' xs ys) xss
    ))) ++ Mul5 _ _ _

... | _ | _ = Mul5 _ _ _
Mul4 _ _ _ {{_}} {{_}} {{_}} = Mul5 _ _ _


-- vector * matrix = vector
Mul5 ._ ._ ._ {{vector A m}} {{matrix B m' n}} {{vector C n'}}
  with m ≟ m' | n ≟ n'
... | yes m=m' | yes n=n' = helper m m' n n' m=m' n=n' where
  helper : (m m' n n' : ℕ) → m ≡ m' → n ≡ n' →
           List (Vec A m → Matrix B m' n → Vec C n')
  helper m .m n .n refl refl with Sum C | Mul (Vec A m) (Vec B m) C
  ... | sums | muls = (sums >>= (λ sum' → flip map muls (λ mul' →
    λ xs yss → Vec.map (mul' xs) (transpose yss)
    ))) ++ Mul6 _ _ _

... | _ | _ = Mul6 _ _ _
Mul5 _ _ _ {{_}} {{_}} {{_}} = Mul6 _ _ _


Mul6 _ _ _ = []


_*_ : {A B C : Set} {{_ : Type A}} {{_ : Type B}} {{_ : Type C}} →
      {{witness : ∃ Mul A B C}} → A → B → C
_*_ = witness (Mul _ _ _)

nat*nat : 3 * 5 ≡ 15
nat*nat = refl

m4 : Matrix ℕ 2 2
m4 = (1 ∷ 2 ∷ []) ∷ (3 ∷ 4 ∷ []) ∷ []

m4*m4 : m4 * m4 ≡ id {A = Matrix ℕ 2 2} ((7 ∷ 10 ∷ []) ∷ (15 ∷ 22 ∷ []) ∷ [])
m4*m4 = refl

m6 : Matrix ℕ 2 3
m6 = (1 ∷ 2 ∷ 3 ∷ []) ∷ (4 ∷ 5 ∷ 6 ∷ []) ∷ []

m6*m6T : m6 * transpose m6 ≡ id {A = Matrix ℕ 2 2} ((14 ∷ 32 ∷ []) ∷ (32 ∷ 77 ∷ []) ∷ [])
m6*m6T = refl

m6T*m6 : transpose m6 * m6 ≡ id {A = Matrix ℕ 3 3}
                                ( (17 ∷ 22 ∷ 27 ∷ []) ∷
                                  (22 ∷ 29 ∷ 36 ∷ []) ∷
                                  (27 ∷ 36 ∷ 45 ∷ []) ∷ [] )
m6T*m6 = refl

v2 : Vec ℕ 2
v2 = 1 ∷ 2 ∷ []

v2*v2 : v2 * v2 ≡ 5
v2*v2 = refl

m4*v2 : m4 * v2 ≡ id {A = Vec ℕ 2} (5 ∷ 11 ∷ [])
m4*v2 = refl

v2*m4 : v2 * m4 ≡ id {A = Vec ℕ 2} (7 ∷ 10 ∷ [])
v2*m4 = refl
