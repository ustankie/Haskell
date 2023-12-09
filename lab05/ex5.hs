{-# LANGUAGE DeriveFunctor #-}
import Data.Graph (Tree(Node))
newtype Box a = MkBox a deriving Show


data MyList a = EmptyList
              | Cons a (MyList a) deriving (Show,Functor)

data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving (Show)

instance Functor BinTree where
    fmap _ EmptyBT=EmptyBT
    fmap f (NodeBT a left right )=NodeBT (f a) (fmap f left) (fmap f right)


instance Applicative Box where
  pure = MkBox
  (<*>) :: Box (a -> b) -> Box a -> Box b
  (MkBox f) <*> w = fmap f w

instance Functor Box where
  fmap f (MkBox x) = MkBox (f x)

newtype MyTriple a = MkTriple (a,a,a) deriving (Show)

instance Functor MyTriple where
    fmap f (MkTriple(a,b,c))=MkTriple(f a, f b, f c)

instance Applicative MyTriple where
    pure a = MkTriple (a, a, a)
    (MkTriple (f, g, h)) <*> (MkTriple (x, y, z)) = MkTriple (f x, g y, h z)


