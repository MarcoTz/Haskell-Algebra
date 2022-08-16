{-# LANGUAGE FlexibleInstances #-}
module Field where

import Data.Ratio

class Field a where 
  ( + ) :: a->a->a
  ( - ) :: a->a->a
  ( * ) :: a->a->a
  ( / ) :: a->a->a
  inv :: a -> a 
  oneElem :: a
  zeroElem :: a

type QQ = Ratio Integer 

instance Field QQ where 
  (+)  = (Prelude.+)
  (-) = (Prelude.-)
  (*)  = (Prelude.*)
  (/) = (Prelude./)
  inv  = (Prelude./ 1)
  oneElem = 1
  zeroElem = 0

type RR = Double

instance Field RR where 
  (+) = (Prelude.+)
  (*) =  (Prelude.*)
  (-) = (Prelude.-)
  (/) = (Prelude./)
  inv = (Prelude./ 1)
  oneElem = 1
  zeroElem = 0 

data CC = Pair RR RR
  deriving (Ord, Eq, Show)

multC :: CC -> CC -> CC
multC (Pair a1 b1) (Pair a2 b2) = 
  Pair ((a1 Field.* a2) Field.+ (b1 Field.* b2)) ((a1 Field.*b2) Field.+ (b1 Field.* a1))

re :: CC -> RR 
re (Pair a _) = a

im :: CC -> RR 
im (Pair _ a) = a

 
instance Field CC where 
  a*b = multC a b
  a/b = multC a (inv b)
  a+b = Pair (re a Field.+ re b) (im a Field.+ im b)
  a-b = Pair (re a Field.- re b) (im a Field.- im b)
  inv a = Pair 1 0 Field./ a
  oneElem = Pair 1 0
  zeroElem = Pair 0 0 
