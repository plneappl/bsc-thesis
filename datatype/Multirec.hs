{-# LANGUAGE RankNTypes, GADTs, GeneralizedNewtypeDeriving
  , KindSignatures
  #-}


-- EXPERIMENT OF FIXED POINT REPRESENTATION FOR MUTUALLY
-- RECURSIVE DATATYPES

-- based on:
-- Rodriguez Yakushev et al.
-- Generic programming with fixed points for mutually recursive datatypes.

import Prelude hiding (sum)

data HFix f ix = HIn (f (HFix f) ix)

data E
data O

data EOX ix where
  E :: EOX E
  O :: EOX O

data EOF :: (* -> *) -> * -> * where
  Zero :: EOF eof E
  Esuc :: Int -> eof O -> EOF eof E
  Osuc :: Int -> eof E -> EOF eof O

zero   = HIn Zero
esuc x = HIn . Esuc x
osuc x = HIn . Osuc x

type Eve = HFix EOF E
type Odd = HFix EOF O

class HFunctor f where
  hmap :: (forall ix. r ix -> r' ix) -> f r ix -> f r' ix

instance HFunctor EOF where
  hmap f Zero = Zero
  hmap f (Esuc x o) = Esuc x (f o)
  hmap f (Osuc x e) = Osuc x (f e)

type Algebra   f r = forall ix. f r ix -> r ix
type Coalgebra f r = forall ix. r ix -> f r ix

cata :: HFunctor f => Algebra f r -> HFix f ix -> r ix
cata f (HIn t) = f (hmap (cata f) t)

ana :: HFunctor f => Coalgebra f r -> r ix -> HFix f ix
ana f x = HIn $ hmap (ana f) (f x)

newtype Const a b = Const { unConst :: a }
  deriving Num
  -- nevertheless, + doesn't work between `C Int E` and `C Int O`.

sum :: Eve -> Int
sum = unConst . cata algebra where
  algebra :: Algebra EOF (Const Int)
  algebra Zero = Const 0
  algebra (Esuc x (Const o)) = Const (x + o)
  algebra (Osuc x (Const e)) = Const (x + e)

xs :: Eve
xs = esuc 1 $ osuc 2 $ esuc 3 $ osuc 4 $ zero

-- sum xs == 10


-- CONCRETE AND ABSTRACT GRAMMARS AS DATATYPES IN TERMS OF HFIX

data C
data S
data F
data A

data Add = Add
data Mul = Mul
data LP  = LP
data RP  = RP

data ConF conf ix where
  C1 :: conf C -> Add -> conf S -> ConF conf C
  C2 :: conf S -> ConF conf C
  S3 :: conf S -> Mul -> conf F -> ConF conf S
  S4 :: conf F -> ConF conf S
  F5 :: Int -> ConF conf F
  F6 :: LP -> conf C -> RP -> ConF conf F

data AbsF absf ix where
  A1 :: absf ix -> Add -> absf ix -> AbsF absf ix
  A3 :: absf ix -> Mul -> absf ix -> AbsF absf ix
  A5 :: Int -> AbsF absf ix

type Concrete = HFix ConF
type Abstract = HFix AbsF

instance HFunctor ConF where
  hmap f (C1 x Add y) = C1 (f x) Add (f y)
  hmap f (C2 x) = C2 (f x)
  hmap f (S3 x Mul y) = S3 (f x) Mul (f y)
  hmap f (S4 x) = S4 (f x)
  hmap f (F5 n) = F5 n
  hmap f (F6 LP x RP) = F6 LP (f x) RP

instance HFunctor AbsF where
  hmap f (A1 x Add y) = A1 (f x) Add (f y)
  hmap f (A3 x Mul y) = A3 (f x) Mul (f y)
  hmap f (A5 n) = A5 n


-- SYNTAX TREE TRANSFORMATIONS AS ALGEBRAS

forwardA :: Algebra ConF (Const (Abstract a))
forwardA (C1 (Const x) Add (Const y)) = Const . HIn $ A1 x Add y
forwardA (C2 (Const x)) = Const x
forwardA (S3 (Const x) Mul (Const y)) = Const . HIn $ A3 x Mul y
forwardA (S4 (Const x)) = Const x
forwardA (F5 n) = Const . HIn $ A5 n
forwardA (F6 LP (Const x) RP) = Const x

forward :: Concrete a -> Abstract a
forward = unConst . cata forwardA

-- Coalgebraic view is harder. Not sure if possible to encode in Haskell.
-- forwardC :: Coalgebra AbsF ()

