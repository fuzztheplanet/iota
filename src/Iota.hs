-- | Re-exports, so that one @import Iota@ gets you the types, the
-- interpreter and the reader and printer entry points.
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
