import qualified Data.Foldable as F  
a=do
    s<-getLine
    n <- return 3
    putStrLn $ s ++ show n

a1=getLine>>=
    \s->return 3 >>=
        \n->putStrLn $ s++show n


data Tree a=Node a (Tree a) (Tree a) 
        | Leaf deriving (Show, Read, Eq,Foldable)


treeExample :: Tree Integer
treeExample =Node 1 (Node 2 Leaf Leaf)  (Node 3 (Node 4 Leaf Leaf) (Node 5 Leaf Leaf))


-- (1)a) `fmap` b) <*> c) <* d) *> e) <$ f) `pure`
-- (2)a) <$> b) `pure` c)>>= d) <* e)*> f) <*>
-- (3)a)fmap paths b)paths c)paths *> d)paths <*> e) paths <$

instance Functor Tree where 
    fmap _ Leaf=Leaf
    fmap f (Node x left right)=Node (f x) (fmap f left) (fmap f right)

instance Applicative Tree where 
    pure :: a -> Tree a
    pure x = Node x Leaf Leaf
    Leaf <*> _ =Leaf
    _ <*> Leaf=Leaf
    (Node f fl fr) <*> (Node g gl gr)= Node ( f g) (fl <*> gl) (fr <*> gr)

paths :: Tree a -> [a]
paths Leaf = []
paths (Node a lt rt) = concat $ (([(a+)] `pure`) <$> (fmap paths [lt, rt]))
-- instance F.Foldable Tree where  
--     foldMap f Leaf= mempty  
--     foldMap f (Node x l r) = F.foldMap f l `mappend`  
--                              f x           `mappend`  
--                              F.foldMap f r 

-- main :: IO ()
-- main = do
--     let treeExample1 = Node (1 (Node 2 (Leaf) (Leaf )) Leaf)
--     let flattenedList = F.concat treeExample1
--     print flattenedList