polarToCartesian :: Floating a=> (a,a)->(a,a)
polarToCartesian (r,phi)=(r*cos phi,r*sin phi)

type CartesianCoord' a=(a,a)
type PolarCoord' a=(a,a)

polarToCartesian' :: Floating a=>PolarCoord' a->CartesianCoord' a
polarToCartesian' (r,phi)=(r*cos phi,r*sin phi)

newtype CartesianCoord'' a=MkCartesianCoord''(a,a) deriving (Show)
newtype PolarCoord'' a=MkPolarCoord'' (a,a) deriving (Show)

polarToCartesian'' :: Floating a=>PolarCoord'' a->CartesianCoord'' a
polarToCartesian'' (MkPolarCoord'' (r,phi))=MkCartesianCoord'' (r*cos phi,r*sin phi)



type Name' = String
type Surname' = String
type Address' = String
type PersonInfo' = (Name', Surname', Address')
type PersonInfoToStringType' = PersonInfo' -> String

personInfoToString' :: PersonInfoToStringType'
personInfoToString' (name, surname, address)=name++" "++surname++" "++address

newtype Name=Name String 
newtype Surname =Surname String
newtype Address = Address String
newtype PersonInfo = PersonInf (Name', Surname', Address') deriving (Show)
newtype PersonInfoToStringType = PersonInfoToStringType (PersonInfo -> String)


personInfoToString'' :: PersonInfo -> String
personInfoToString''  (PersonInf (name,surname,address))=name++" "++surname++" "++address