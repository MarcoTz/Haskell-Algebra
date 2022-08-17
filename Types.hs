module Types where 

import Data.Ratio

-- Fields
type QQ = Ratio Integer
type RR = Double
data CC = Pair RR RR 
  deriving (Eq, Show)

-- Rings
type ZZ = Integer

--Monoids
data NN = Z | Succ NN
  deriving (Ord, Eq, Show)

-- Functions for NN 
nat :: Integer -> NN 
nat 0 = Z 
nat n = Succ (nat (n-1))

int :: NN -> Integer
int Z = 0 
int (Succ n) = int n + 1

-- Functions for CC
multC :: CC -> CC -> CC
multC (Pair a1 b1) (Pair a2 b2) = 
  Pair ((a1 * a2) + (b1 * b2)) ((a1 *b2)+ (b1 * a1))

re :: CC -> RR 
re (Pair a _) = a

im :: CC -> RR 
im (Pair _ a) = a

conjugate :: CC -> CC 
conjugate (Pair a b) = Pair a (-1 * b)

fromPolar :: RR -> RR -> CC 
fromPolar len angle = Pair (len * cos angle) (len * sin angle)

toPolar :: CC -> (RR,RR)
toPolar (Pair a b) = (sqrt ((a *a)+(b *b)), atan (b / a))
