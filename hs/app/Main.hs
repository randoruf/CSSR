{-# LANGUAGE RecordWildCards #-}
module Main where

import Prelude
import Options.Applicative (execParser)

import Cli

main :: IO ()
main = execParser opts >>= runCssr

runCssr :: Arguments -> IO ()
runCssr Arguments{..} = undefined


