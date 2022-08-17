{-# LANGUAGE FlexibleInstances #-}
module Field where

import Types
import Ring
import Data.Ratio
import qualified Prelude as P

class (Ring a) => Field a where 
  ( / ) :: a->a->a
  x / y = x * inv y
  inv :: a -> a 
  

instance Field QQ where 
  inv  = (P./ 1)

instance Field RR where 
  inv = (P./ 1)

instance Field CC where 
  inv a = Pair 0 1 / a
