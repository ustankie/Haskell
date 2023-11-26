import Data.Graph (Tree(Node))
data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a) deriving Show

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt


data Expr a = Lit a |Add (Expr a) (Expr a)|Mul (Expr a) (Expr a) deriving Show

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2)=(eval e1)* (eval e2)

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
show' (Mul e1 e2) = "(" ++ show' e1 ++ "*" ++ show' e2 ++ ")"

depthOfBT :: BinTree a -> Int
depthOfBT EmptyBT=0
depthOfBT (NodeBT x left right)=1+max (depthOfBT (left)) (depthOfBT (right))

flatPre :: BinTree a -> [a]
flatPre EmptyBT=[]
flatPre (NodeBT x left right)=x:((flatPre left)++(flatPre right))

flatIn :: BinTree a -> [a]
flatIn EmptyBT=[]
flatIn (NodeBT x left right)=(flatIn left)++[x]++(flatIn right)

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT f EmptyBT=EmptyBT
mapBT f (NodeBT x left right)=NodeBT (f x) (mapBT f left) (mapBT f right)


insert :: Ord a => a -> BinTree a -> BinTree a
insert x EmptyBT=(NodeBT x EmptyBT EmptyBT)
insert x (NodeBT n left right)
                |x==n=(NodeBT n left right)
                |x>n=(NodeBT n left (insert x right))
                |x<n=(NodeBT n (insert x left)  right)

list2BST :: Ord a => [a] -> BinTree a
list2BST []=EmptyBT
list2BST (x:xs)=insert x (list2BST xs)