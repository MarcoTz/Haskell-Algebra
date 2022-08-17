{-# LANGUAGE FlexibleInstances #-}

module Group where

import qualified Prelude as P
import Types

class Group a where 
  (+),(-) :: a->a->a
  x-y = x + neg y
  neg :: a->a

instance Group QQ where 
  (+) = (P.+)
  neg x = -1 P.* x

instance Group RR where
  (+) = (P.+)
  neg x = -1 P.* x

instance Group CC where 
  a+b = Pair (re a + re b) (im a + im b)
  neg x = Pair (neg (re x)) (neg (im x))

instance Group ZZ where 
  (+) = (P.+)
  neg x = -1 P.* x

