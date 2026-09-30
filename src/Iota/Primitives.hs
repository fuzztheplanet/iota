-- | Concatenates the primitive tables from the modules below so the
-- interpreter can install them in one go. Control and IO need the
-- runtime, which is why this takes one.
module Iota.Primitives
  ( primitiveBindings
  ) where

import Iota.Primitives.Control (controlPrimitives)
import Iota.Primitives.IO (ioPrimitives)
import Iota.Primitives.Numeric (numericPrimitives)
import Iota.Primitives.Pairs (pairPrimitives)
import Iota.Primitives.Predicates (predicatePrimitives)
import Iota.Primitives.Strings (stringPrimitives)
import Iota.Primitives.Vectors (vectorPrimitives)
import Iota.Types

primitiveBindings :: Runtime -> [(Symbol, Value)]
primitiveBindings rt =
  concat
    [ numericPrimitives
    , pairPrimitives
    , predicatePrimitives
    , stringPrimitives
    , vectorPrimitives
    , controlPrimitives rt
    , ioPrimitives rt
    ]
