module Queue
  ( Queue
  , emptyQ   -- :: Queue a
  , isEmptyQ -- :: Queue a -> Bool
  , addQ     -- :: a -> Queue a -> Queue a
  , remQ     -- :: Queue a -> (a, Queue a)
  ) where

emptyQ :: Queue a
isEmptyQ :: (Queue a)->Bool
addQ:: a ->(Queue a)-> Queue a
remQ ::Queue a -> (a,(Queue a))

newtype Queue a=MkQueue [a] deriving Show

emptyQ=MkQueue []
isEmptyQ (MkQueue a)=null a
addQ x (MkQueue a)=(MkQueue (a++[x]))
remQ (MkQueue (x:xs))=(x,(MkQueue xs))


