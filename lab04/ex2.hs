
data CartInt2DVec = MkCartInt2DVec Int Int deriving(Show)

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y

data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y

data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a} deriving(Show)

data List a=EmptyL | Cons a (List a) deriving Show

head'::List a->a
head' EmptyL =error "head': the empty list has no head!"
head' (Cons x xs)=x

data ThreeColors = Blue |
                   White |
                   Red

type ActorName = String
leadingActor :: ThreeColors -> ActorName
leadingActor Blue  = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red   = "Irene Jacob"

data Cart3DVec a = MkCart3DVec a a a deriving Show

xCoord3D:: Cart3DVec a->a
xCoord3D (MkCart3DVec a _ _)=a

data Cart3DVec' a = MkCart3DVec' {x1::a,y1::a,z1::a} deriving Show


data Shape = Circle Float |
             Rectangle Float Float

area :: Shape -> Float
area (Circle r)=pi*r^2
area (Rectangle a b)=a*b

data TrafficLights=Red1 |Green1| Yellow1 deriving (Show,Enum)

actionFor :: TrafficLights -> String
actionFor Red1="stop"
actionFor Yellow1="prepare"
actionFor Green1="go"


data Foo a=MkFoo {value:: a, name :: String}

instance Show a=>  Show (Foo a) where
    show (MkFoo {value=v, name=n})="Foo"++show n++show v


data Tree a=Node (Tree a) a (Tree a)|Leaf 

toList:: Tree a->[a]
toList (Leaf )=[]
toList (Node left x right)=toList left ++ [x]++toList right

sumSq :: Num a=>Tree a->a
sumSq Leaf=0
sumSq (Node left x right)=x^2+ sumSq left +sumSq right

data Box a=MkBox {val::a}

instance Show a=> Show (Box a) where
    show (MkBox {val=v})="Box with "++show v


data Data=Data{first::String,second::String} deriving Show
