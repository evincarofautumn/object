{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Object where

data Pair a b = Pair a b
  deriving (Show)

data GetX = GetX
data GetY = GetY
data SetX a = SetX a
data SetY a = SetY a

class Accepts a s where
  type Result a s
  (#) :: a -> s -> Result a s

infixl 9 #

instance Accepts (Pair a b) GetX where
  type Result (Pair a b) GetX = a
  Pair x _ #GetX = x

instance Accepts (Pair a b) GetY where
  type Result (Pair a b) GetY = b
  Pair _ y #GetY = y

instance (a1 ~ a2) => Accepts (Pair a1 b) (SetX a2) where
  type Result (Pair a1 b) (SetX a2) = Pair a2 b
  Pair x y #SetX x' = Pair x' y

instance (b1 ~ b2) => Accepts (Pair a b1) (SetY b2) where
  type Result (Pair a b1) (SetY b2) = Pair a b2
  Pair x y #SetY y' = Pair x y'

-- Example: Pair 0 0 #SetX 1 #SetY 2 == Pair 1 2
