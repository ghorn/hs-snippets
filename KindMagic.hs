{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module KindMagic ( Alpha(..)
                 , getUnsafe
                 , getSafe
                 , getAnything
                 ) where

data Safety = IsSafe | IsUnsafe

data Alpha (safetype :: Safety) where
  SafeThing    :: Int -> Alpha IsSafe
  UnsafeThing  :: Int -> Alpha IsUnsafe
  UnknownThing :: Int -> Alpha safetype
  
instance Show (Alpha a) where
  show (SafeThing k)   = "SafeThing "++ show k
  show (UnsafeThing k) = "UnsafeThing "++ show k
  show (UnknownThing k) = "UnknownThing "++ show k

getUnsafe :: Alpha IsUnsafe -> Int
getUnsafe (UnsafeThing k)  = k
getUnsafe (UnknownThing k) = k

getSafe :: Alpha IsSafe -> Int
getSafe (SafeThing k)    = k
getSafe (UnknownThing k) = k

getAnything :: Alpha a -> Int
getAnything (SafeThing k)    = k
getAnything (UnsafeThing k)  = k
getAnything (UnknownThing k) = k

-- wowItsAnError :: Alpha IsUnsafe -> Int
-- wowItsAnError (SafeThing k)    = k
-- wowItsAnError (UnsafeThing k)  = k
-- wowItsAnError (UnknownThing k) = k

