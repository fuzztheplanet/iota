-- | The real main is in the library, where the tests can reach it.
module Main (main) where

import Iota.CLI qualified

main :: IO ()
main = Iota.CLI.main
