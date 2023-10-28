isPalindrome :: [Char] -> Bool
isPalindrome s =(reverse s)==s

getElemAtIdx :: [a]->Int->  a
getElemAtIdx s b = head(drop b s)

capitalize :: [Char] ->[Char]
capitalize w|h>=97 && h<=122 =(toEnum (h-32)):tail w
            |otherwise=w
            where h=fromEnum (head w)

capitalize2 :: [Char] ->[Char]
capitalize2 (x:w)|h>=97 && h<=122 =(toEnum (h-32)):w
            |otherwise=x:w
            where h=fromEnum (x)