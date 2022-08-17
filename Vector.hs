{-# LANGUAGE GADTs #-}
module Vector where 

import Field
import Ring
import Types
import Group
import qualified Prelude as P

data Vector a where 
 Vempty :: (Field a) => Vector a
 Vcons :: (Field a) => a -> Vector a -> Vector a 

instance (Field a) => Group (Vector a) where 
 (+) = vAdd
 neg = scalarMult (neg oneElem)
 

vAdd :: Field a => Vector a -> Vector a -> Vector a
vAdd Vempty Vempty = Vempty
vAdd Vempty _ = P.error "Vectors must have the same dimension" 
vAdd _ Vempty = P.error "Vectors must have the same dimension"
vAdd (Vcons a1 vec1) (Vcons a2 vec2) = Vcons (a1+a2) (vAdd vec1 vec2)

scalarMult :: Field a => a -> Vector a -> Vector a
scalarMult _ Vempty = Vempty
scalarMult x (Vcons a vec) = Vcons (x*a) (scalarMult x vec)

dim :: Field a => Vector a -> NN
dim Vempty = Z
dim (Vcons _ vec) = Succ (dim vec)

append :: Field a => a -> Vector a -> Vector a 
append x Vempty = Vcons x Vempty 
append x (Vcons y vec) = Vcons y (append x vec)

prepend :: Field a => a -> Vector a -> Vector a
prepend = Vcons 

innerProd :: Field a => Vector a -> Vector a -> a 
innerProd Vempty Vempty = zeroElem
innerProd Vempty _ = P.error "Vectors do not have the same dimension"
innerProd _ Vempty = P.error "Vectors do not have the same dimension"
innerProd (Vcons a1 vec1) (Vcons a2 vec2) = a1*a2 + innerProd vec1 vec2
