-- | Re-exports, so that one @import Iota@ gets you the types, the
-- interpreter and the reader and printer entry points. A Haskell
-- program that runs Scheme with iota needs this module and no other.
-- The rest of the library is the inside of the interpreter: reader,
-- expander, evaluator, printer and primitives, each exposed so that it
-- can be tested on its own.
--
-- = Using the interpreter from Haskell
--
-- An 'Interpreter' is made once with 'newInterpreter' and keeps its
-- definitions from one call to the next. 'runText' runs a program
-- given as text. It returns the value of the last form, or the message
-- when an error got as far as the top level. Its 'FilePath' is the
-- name that messages use. A name that starts with @<@ says the text
-- did not come from a file, and an error raised while running is then
-- reported without a file position.
--
-- This is @examples\/haskell\/Embed.hs@. The test suite builds it, runs
-- it and compares what it prints with the lines below it.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > -- | iota inside a Haskell program: an interpreter that keeps its
-- > -- definitions from one call to the next, a value turned into text, an
-- > -- error that comes back as text, and output that is captured.
-- > module Embed (main) where
-- >
-- > import qualified Data.Text.IO as TIO
-- > import Iota
-- >
-- > main :: IO ()
-- > main = do
-- >   interp <- newInterpreter
-- >   -- Definitions stay in the interpreter from one call to the next.
-- >   _ <- runText interp "<setup>" "(define (square x) (* x x))"
-- >   result <- runText interp "<main>" "(square 12)"
-- >   case result of
-- >     Left err -> TIO.putStrLn ("error: " <> err)
-- >     Right value -> writeText value >>= TIO.putStrLn
-- >   -- An error comes back as text, not as an exception.
-- >   failed <- runText interp "<main>" "(car 1)"
-- >   print (either id (const "no error") failed)
-- >   -- Output goes to the real stdout unless it is captured.
-- >   (shown, output) <-
-- >     captureOutput interp $
-- >       runTextForOutput interp "<main>" "(display \"hi\") (+ 1 2)"
-- >   print (shown, output)
--
-- That program prints
--
-- > 144
-- > "expected a pair, got: 1"
-- > ("3","hi")
--
-- A 'Value' is a mutable object, so turning one into text is an IO
-- action. 'writeText' gives the @write@ form and 'displayText' the
-- @display@ form. 'runTextForOutput' is 'runText' followed by
-- 'writeText', with the message in place of the value when there was an
-- error. 'runFile' does the same as 'runText' for a file on disk. The
-- one exception all of these let through is @ExitCode@ from
-- @System.Exit@, which a program that calls @exit@ throws on purpose.
--
-- 'readDatums' and 'readDatum' are the reader on its own. They turn
-- text into 'Datum' without evaluating anything.
--
-- The user's guide says how a package of your own gets iota as a
-- dependency: <https://fuzztheplanet.github.io/iota/guide.html#using-iota-from-haskell>.
module Iota
  ( module Iota.Types
  , module Iota.Interpreter
  , readDatums
  , readDatum
  , writeText
  , displayText
  ) where

import Iota.Interpreter
import Iota.Printer (displayText, writeText)
import Iota.Reader (readDatum, readDatums)
import Iota.Types
