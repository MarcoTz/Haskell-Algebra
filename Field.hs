{-# LANGUAGE FlexibleInstances #-}
module Field where

import Types
import Data.Ratio
import qualified Prelude as P

class Field a where 
  ( + ) :: a->a->a
  ( - ) :: a->a->a
  x - y = x + neg y
  ( * ) :: a->a->a
  ( / ) :: a->a->a
  x / y = x * inv y
  neg :: a->a
  inv :: a -> a 
  oneElem :: a
  zeroElem :: a


instance Field QQ where 
  (+)  = (P.+)
  (*)  = (P.*)
  neg = (P.- 0)
  inv  = (P./ 1)
  oneElem = 1
  zeroElem = 0

instance Field RR where 
  (+) = (P.+)
  (*) =  (P.*) 
  neg = (P.- 0)
  inv = (P./ 1)
  oneElem = 1
  zeroElem = 0 

instance Field CC where 
  a*b = multC a b
  a+b = Pair (re a + re b) (im a + im b)
  neg a = Pair (neg (re a)) (neg (im a))
  inv a = Pair 0 1 / a
  oneElem = Pair 1 0
  zeroElem = Pair 0 0 
