{-# OPTIONS_GHC -Wall #-}
{-# Language ForeignFunctionInterface #-}

module Main where

import Foreign.C(CDouble(..))

foreign import ccall unsafe "do_something.h multiply" c_multiply :: CDouble -> CDouble -> IO CDouble

main :: IO ()
main = c_multiply 3 4 >>= print
