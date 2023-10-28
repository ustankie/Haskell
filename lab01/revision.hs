-- import Data.Char(ord,chr)
absInt :: Int -> Int
absInt x=if x>=0
    then x
    else -x

min2Int :: (Int,Int) -> Int
min2Int (x,y)= if x<y
    then x
    else y

min3Int :: (Int,Int,Int) -> Int
min3Int (x,y,z)|x<=y && x<=z =x
            |y<=z && y<=x=y
            |otherwise=z

toUpper:: Char -> Char
toUpper x | ascii>=97 && ascii<=122 = toEnum(ascii-32) 
        |otherwise =x
        where ascii=fromEnum x
    
isDigit :: Char -> Bool
isDigit x|ascii>=48 && ascii<=57 =True
        |otherwise=False
        where ascii=fromEnum x

charToNum :: Char -> Int
charToNum x=fromEnum x


isItTheAnswer :: String -> Bool
isItTheAnswer "Love"=True
isItTheAnswer _=False

nand' :: (Bool,Bool) -> Bool
nand' (True,True) =False
nand' _=True

and' :: (Bool,Bool) -> Bool
and' (x,y)=case (x,y) of
    (True,True) -> True
    _ -> False

xor' (x,y)=case (x==y) of
        True ->True
        _ ->False

triangle :: (Double,Double,Double) -> Double
triangle (a,b,c)=sqrt(p*(p-a)*(p-b)*(p-c))
        where p=0.5*(a+b+c)

unitVec3D (a,b,c)=
        let d=sqrt(a^2+b^2+c^2)
        in (a/d,b/d,c/d)
roots ::(Double,Double,Double)-> (Double,Double)
roots (a,b,c)=((-b-d)/e,(-b+d)/e)
    where d=sqrt(b^2-4*a*c)
          e=2*a

