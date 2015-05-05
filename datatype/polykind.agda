-- experimenting with nonprimitive recursion
-- desugaring `int a b c;` into `int a; int b; int c;`
-- G  = sugared grammar
-- G′ = desugared grammar

open import Data.String

data G : Set
data G′ : Set
data D : Set
data D′ : Set
data T : Set
data U : Set
data V : Set

data G where
  G₁ : D → G → G
  G₂ : G

data G′ where
  G₁′ : D′ → G′ → G′
  G₂′ : G′

data D where
  D₃ : T → U → D

data D′ where
  D₃′ : T → V → D′

data T where
  T₄ : String → T

data U where
  U₅ : V → U → U
  U₆ : V → U

data V where
  V₇ : String → V

desugar : G → G′
desugar (G₁ (D₃ t (U₅ v u)) g) = G₁′ (D₃′ t v) (desugar (G₁ (D₃ t u) g))
desugar (G₁ (D₃ t (U₆ v)) g)   = G₁′ (D₃′ t v) (desugar g)
desugar G₂                     = G₂′

-- even as we can obtain `desugar` by inlining the first recursive call of
-- `desugar′`, the termination checker accepts `desugar` and rejects `desugar′`
{-# NON_TERMINATING #-}
desugar′ : G → G′
desugar′ (G₁ (D₃ t (U₅ v u)) g) = desugar′ (G₁ (D₃ t (U₆ v)) (G₁ (D₃ t u) g))
desugar′ (G₁ (D₃ t (U₆ v)) g)   = G₁′ (D₃′ t v) (desugar′ g)
desugar′ G₂                     = G₂′

-- primitive recursion	vs	general recursion
-- regular datatypes	vs	higher-kinded datatypes
