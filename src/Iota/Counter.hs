{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | A process-wide counter for object identities.
--
-- Every pair, vector, record, port, closure and continuation gets an Int
-- from here. That number is how @eqv?@, @equal?@ and @write@ tell whether
-- they have seen an object before. It is one machine word bumped with an
-- atomic fetch-and-add, which is cheap and also safe when the test suite
-- runs several interpreters at the same time.
module Iota.Counter
  ( freshId
  ) where

import GHC.Exts (Int (I#), MutableByteArray#, RealWorld, fetchAddIntArray#, newByteArray#, writeIntArray#)
import GHC.IO (IO (IO))
import System.IO.Unsafe (unsafePerformIO)

data Counter = Counter (MutableByteArray# RealWorld)

-- newByteArray# hands back memory with whatever was in it, so the
-- counter is set to zero here. Identities have to start at zero: a
-- negative one marks a literal constant.
newCounter :: IO Counter
newCounter = IO $ \s -> case newByteArray# 8# s of
  (# s1, arr #) -> case writeIntArray# arr 0# 0# s1 of
    s2 -> (# s2, Counter arr #)

counter :: Counter
counter = unsafePerformIO newCounter
{-# NOINLINE counter #-}

-- | The next identity. Never repeats within a process.
freshId :: IO Int
freshId = case counter of
  Counter arr -> IO $ \s -> case fetchAddIntArray# arr 0# 1# s of
    (# s', n #) -> (# s', I# n #)
