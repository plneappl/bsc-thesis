open import Data.Nat
open import Relation.Nullary.Core

infixr 5 _⇒_
infixl 9 _!_

data Kind : Set where
  * : Kind
  _⇒_ : Kind → Kind → Kind

data Type : Set where
  s : Type
  t : Type
  v : ℕ → Type
  _!_ : Type → Type → Type
  _⇒_ : Type → Type → Type
  All : Type → Type

if_then_else_ : ∀ {A B : Set} → Dec A → B → B → B
if yes p then th else el = th
if no ¬p then th else el = el

shift : ℕ → ℕ → Type → Type
shift c n s = s
shift c n t = t
shift c n (v x) = if c ≤? x then v (n + x) else v x
shift c n (t₁ ! t₂) = shift c n t₁ ! shift c n t₂
shift c n (t₁ ⇒ t₂) = shift c n t₁ ⇒ shift c n t₂
shift c n (All tpe) = All (shift (c + 1) n tpe)

inc2 : Type → Type
inc2 = shift 0 2


Map : Kind → Type → Type → Type
Map * x y = x ⇒ y
Map (k₁ ⇒ k₂) x y =
  All (Map k₁ (v 0) (v 1) ⇒ Map k₂ (inc2 x ! v 0) (inc2 y ! v 1))


MapFix2 : Type
MapFix2 = Map ((* ⇒ * ⇒ *) ⇒ (* ⇒ * ⇒ *) ⇒ *) s t

{-
MapFix2 =
  = All
    (All
     ((v 0 ⇒ v 1) ⇒
      All ((v 0 ⇒ v 1) ⇒ v 4 ! v 2 ! v 0 ⇒ v 5 ! v 3 ! v 1))
     ⇒
     All
     (All
      ((v 0 ⇒ v 1) ⇒
       All ((v 0 ⇒ v 1) ⇒ v 4 ! v 2 ! v 0 ⇒ v 5 ! v 3 ! v 1))
      ⇒ s ! v 2 ! v 0 ⇒ t ! v 3 ! v 1))
  =
    ∀ f g h k.
    (∀ a b c d. (a → c) → (b → d) → f a b → g c d) →
    (∀ a b c d. (a → c) → (b → d) → h a b → k c d) →
    s f h → t g k
-}

