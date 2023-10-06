-- import Data.Char(ord,chr)
sgn :: Int -> Int
sgn n = if n < 0
    then -1
    else if n == 0
        then 0
        else 1

absInt :: Int -> Int
absInt a = if a >=0
    then a
    else (-a)


min2Int :: (Int, Int) -> Int
min2Int (a,b) = if a < b
    then a
    else b

min3Int :: (Int, Int,Int) -> Int
min3Int (a,b,c) = if (a < b || a == b) && (a < c || a == c)
    then a  
    else if (b<a || b == a) && (b<c || b == c)
        then b    
        else c

min3Int2 :: (Int, Int,Int) -> Int
min3Int2 (a,b,c) = min2Int(min2Int(a,b),c)

toUpper :: Char -> Char
toUpper x = if x >= 'a' && x <= 'z'
    then chr(ord (x) + ord ('A')- ord('a'))
    else x

toUpper2 :: Char -> Char
toUpper2 x | ascii >= 97 && ascii <= 122 = toEnum (ascii-32) :: Char
    |otherwise = x
    where ascii = fromEnum x

toLower :: Char -> Char
toLower x = if x >= 'A' && x <= 'Z'
    then chr(ord (x) - ord ('A') + ord('a'))
    else x

isDigit :: Char -> Bool
isDigit x = if x >= '1' && x <= '9'
    then True
    else False

ord :: Char -> Int
ord x = fromEnum (x)

chr :: Int -> Char
chr x = toEnum x

romanDigit :: Char -> String
romanDigit x
    |ascii==49 ="I"
    |ascii==50="II"
    |ascii==51="III"
    |ascii==52="IV"
    |ascii==53="V"
    |ascii==54="VI"
    |ascii==55="VII"
    |ascii==56="VIII"
    |ascii==57="IX"
    where ascii=fromEnum x



