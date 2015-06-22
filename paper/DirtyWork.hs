module DirtyWork where

import Data.Char

main = putStrLn $ generatedFmts

generatedFmts :: String
generatedFmts = unlines $ formatKeywords ++ formatSubscripts

keywords :: [String]
keywords =
  words $   "begin class data def emit end extends for functor in match new"
    ++" "++ "object out pattern private sealed struct this trait val var"
    ++" "++ "with yield"

subscripts :: [(String, String)]
subscripts =
  [ ("i", "i")
  , ("m", "m")
  , ("n", "n")
  , ("r", "r")
  , ("s", "s")
  , ("n1", "n-1")
  ] ++
  map (\n -> let s = show n in (s, s)) [0..9]

subscribedNames :: [String]
subscribedNames =
  words "A B C D E F f field from Record R S s T t to U V W X Y Z"

formatKeyword :: String -> String
formatKeyword x = "\"\\KEYWORD{" ++ x ++ "}\""

formatKeywords :: [String]
formatKeywords =
  [ "%format " ++ x ++ " = " ++ formatKeyword x
  | x <- keywords
  ]


formatSubscripts :: [String]
formatSubscripts =
  [ "%format " ++ x ++ "_" ++ lhs ++ postfix ++ 
    " = \"\\ensuremath{\\Varid{" ++ x ++ "}_{" ++ rhs ++ "}" ++ prime ++ "}\""
  | x <- subscribedNames
  , (lhs, rhs) <- subscripts
  , (postfix, prime) <- [("", ""), ("prime", "^\\prime")]
  ]

romanIfUpper :: String -> String
romanIfUpper x =
  if isUpper (head x) then
    "\\textrm{" ++ x ++ "}"
  else
    "\\textit{" ++ x ++ "}"
