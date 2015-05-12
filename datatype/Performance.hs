-- experiment of coercion performance

{-# LANGUAGE TemplateHaskell #-}
import Data.Function.Memoize

-- this runs in exponential time.
-- timing `run n` with
-- n=10 : 0.01s
-- n=11 : 0.01s
-- n=12 : 0.02s
-- n=13 : 0.05s
-- n=14 : 0.11s
-- n=15 : 0.25s
-- n=16 : 0.59s
-- n=17 : 1.42s

data List a = Nil | Snoc (List a) a deriving Show

deriveMemoizable ''List

from :: [a] -> List a
from [] = Nil
from (x : xs) = case from xs of
  Snoc ys y -> Snoc (from (x : to ys)) y
  Nil       -> Snoc Nil x

to :: List a -> [a]
to Nil = []
to (Snoc ys y) = case to ys of
  x : xs -> x : to (Snoc (from xs) y)
  []     -> [y]

run :: Int -> Int
run n = last (to (from [1..n]))


-- naive memoization should bring it to O(n²)
-- but this one isn't memoizing appropriately.

mfrom :: Memoizable a => [a] -> List a
mfrom = memoFix tfrom

mto :: Memoizable a => List a -> [a]
mto = memoFix tto

tfrom :: Memoizable a => ([a] -> List a) -> [a] -> List a
tfrom f [] = Nil
tfrom f (x : xs) = case f xs of
  Snoc ys y -> Snoc (f (x : mto ys)) y
  Nil       -> Snoc Nil x

tto :: Memoizable a => (List a -> [a]) -> List a -> [a]
tto t Nil = []
tto t (Snoc ys y) = case t ys of
  x : xs -> x : t (Snoc (mfrom xs) y)
  []     -> [y]

mrun :: Int -> Int
mrun n = last (mto (mfrom [1..n]))
