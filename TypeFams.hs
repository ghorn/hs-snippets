{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

data Zero = Zero deriving Show
data Vec a = Vec [a] deriving Show
data Sca a = Sca a deriving Show
data Mat a = Mat a deriving Show

class Addable a b where
  type Added a b
  add :: a -> b -> Added a b

instance Num a => Addable (Sca a) (Sca a) where
  type Added (Sca a) (Sca a) = Sca a
  add (Sca x) (Sca y)        = Sca (x + y)

instance Num a => Addable (Vec a) (Sca a) where
  type Added (Vec a) (Sca a) = Vec a
  add (Vec xs) (Sca y)       = Vec $ map (+y) xs

instance Num a => Addable (Sca a) (Vec a) where
  type Added (Sca a) (Vec a) = Vec a
  add (Sca x) (Vec ys)       = Vec $ map (x+) ys

instance Addable a Zero where
  type Added a Zero = a
  add x Zero        = x

instance Addable Zero a where
  type Added Zero a = a
  add Zero y        = y

sx,sy :: Sca Double
sx = Sca 10
sy = Sca 20

vx,vy :: Vec Double
vx = Vec [1,2,3,4]
vy = Vec [5,6,7,8]

sz :: Sca Double
sz = add sx sy

vz,vz2 :: Vec Double
vz = add vx sy
vz2 = add sx vy

f = add 1 sx
