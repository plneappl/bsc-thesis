-- experimenting with nonprimitive recursion

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
