{-# LANGUAGE FlexibleInstances #-}

module Ring where 

import qualified Prelude as P
import Types

class Ring a where 
  ( + ) :: a->a->a
  ( - ) :: a->a->a
  x - y = x + neg y
  ( * ) :: a->a->a
  neg :: a->a
  oneElem :: a
  zeroElem :: a

instance Ring QQ where
 (+) = (P.+)
 (*) = (P.*)
 neg x = -1*x 
 oneElem = 1
 zeroElem = 0


instance Ring RR where 
 (+) = (P.+)
 (*) = (P.*)
 neg x = -1*x 
 oneElem = 1
 zeroElem = 0

instance Ring CC where 
  a*b = multC a b
  a+b = Pair (re a + re b) (im a + im b)
  neg a = Pair (neg (re a)) (neg (im a))
  oneElem = Pair 1 0
  zeroElem = Pair 0 0  

instance Ring ZZ where
  (*) = (P.*)
  (+) = (P.+)
  neg x = -1*x
  oneElem = 1
  zeroElem = 0 
