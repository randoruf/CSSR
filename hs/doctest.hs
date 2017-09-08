module Main where

import Test.DocTest

main = doctest
  [ "-isrc", "src/CSSR.hs"
  , "-XTypeFamilies"
  , "-XLambdaCase"
  , "-XViewPatterns"
  , "-XOverloadedStrings"
  ]


