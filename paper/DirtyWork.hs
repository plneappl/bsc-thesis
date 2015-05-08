module DirtyWork where

import Data.Char

main = putStrLn $ generatedFmts

generatedFmts :: String
generatedFmts = unlines $ formatKeywords ++ formatSubscripts

keywords :: [String]
keywords =
  [ "class"
  , "data"
  , "def"
  , "emit"
  , "extends"
  , "for"
  , "functor"
  , "match"
  , "new"
  , "object"
  , "pattern"
  , "private"
  , "sealed"
  , "struct"
  , "this"
  , "trait"
  , "val"
  , "var"
  , "with"
  , "yield"
  ]

subscripts :: [(String, String)]
subscripts =
  [ ("i", "i")
  , ("m", "m")
  , ("n", "n")
  , ("n1", "n-1")
  ] ++
  map (\n -> let s = show n in (s, s)) [0..9]

subscribedNames :: [String]
subscribedNames = ["A", "B", "F", "f", "field", "Record", "S", "T", "t"]

formatKeyword :: String -> String
formatKeyword x = "\"\\KEYWORD{" ++ x ++ "}\""

formatKeywords :: [String]
formatKeywords =
  [ "%format " ++ x ++ " = " ++ formatKeyword x
  | x <- keywords
  ]


formatSubscripts :: [String]
formatSubscripts =
  [ "%format " ++ x ++ "_" ++ lhs ++
    " = \"\\ensuremath{" ++ x ++ "_{" ++ rhs ++ "}}\""
  | x <- subscribedNames
  , (lhs, rhs) <- subscripts
  ]

romanIfUpper :: String -> String
romanIfUpper x =
  if isUpper (head x) then
    "\\textrm{" ++ x ++ "}"
  else
    "\\textit{" ++ x ++ "}"
