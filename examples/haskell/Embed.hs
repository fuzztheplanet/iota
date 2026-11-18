{-# LANGUAGE OverloadedStrings #-}

-- | iota inside a Haskell program: an interpreter that keeps its
-- definitions from one call to the next, a value turned into text, an
-- error that comes back as text, and output that is captured.
module Embed (main) where

import qualified Data.Text.IO as TIO
import Iota

main :: IO ()
main = do
  interp <- newInterpreter
  -- Definitions stay in the interpreter from one call to the next.
  _ <- runText interp "<setup>" "(define (square x) (* x x))"
  result <- runText interp "<main>" "(square 12)"
  case result of
    Left err -> TIO.putStrLn ("error: " <> err)
    Right value -> writeText value >>= TIO.putStrLn
  -- An error comes back as text, not as an exception.
  failed <- runText interp "<main>" "(car 1)"
  print (either id (const "no error") failed)
  -- Output goes to the real stdout unless it is captured.
  (shown, output) <-
    captureOutput interp $
      runTextForOutput interp "<main>" "(display \"hi\") (+ 1 2)"
  print (shown, output)
