{-# LANGUAGE FlexibleInstances #-}
module Field where

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


type QQ = Ratio P.Integer 

instance Field QQ where 
  (+)  = (P.+)
  (*)  = (P.*)
  neg = (P.- 0)
  inv  = (P./ 1)
  oneElem = 1
  zeroElem = 0

type RR = P.Double

instance Field RR where 
  (+) = (P.+)
  (*) =  (P.*) 
  neg = (P.- 0)
  inv = (P./ 1)
  oneElem = 1
  zeroElem = 0 

data CC = Pair RR RR
  deriving (P.Ord, P.Eq, P.Show)

multC :: CC -> CC -> CC
multC (Pair a1 b1) (Pair a2 b2) = 
  Pair ((a1 * a2) + (b1 * b2)) ((a1 *b2)+ (b1 * a1))

re :: CC -> RR 
re (Pair a _) = a

im :: CC -> RR 
im (Pair _ a) = a

conjugate :: CC -> CC 
conjugate (Pair a b) = Pair a (neg b)

fromPolar :: RR -> RR -> CC 
fromPolar len angle = Pair (len * P.cos angle) (len * P.sin angle)

toPolar :: CC -> (RR,RR)
toPolar (Pair a b) = (P.sqrt ((a *a)+(b *b)), P.atan (b / a))

instance Field CC where 
  a*b = multC a b
  a+b = Pair (re a + re b) (im a + im b)
  neg a = Pair (neg (re a)) (neg (im a))
  inv a = Pair 0 1 / a
  oneElem = Pair 1 0
  zeroElem = Pair 0 0 
