{-# LANGUAGE FlexibleInstances #-}

module Ring where 

import qualified Prelude as P
import Types
import Group

class (Group a) => Ring a where 
  ( - ) :: a->a->a
  x - y = x + neg y
  ( * ) :: a->a->a
  oneElem :: a
  zeroElem :: a

instance Ring QQ where
 (*) = (P.*)
 oneElem = 1
 zeroElem = 0


instance Ring RR where 
 (*) = (P.*)
 oneElem = 1
 zeroElem = 0

instance Ring CC where 
  a*b = multC a b
  oneElem = Pair 1 0
  zeroElem = Pair 0 0  

instance Ring ZZ where
  (*) = (P.*)
  oneElem = 1
  zeroElem = 0 
