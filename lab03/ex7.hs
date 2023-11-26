import Text.XHtml.Frameset (p)
import Distribution.Simple.Utils (xargs)
onlyEven []=[]
onlyEven (x:xs)
    | x `mod` 2==0=x:onlyEven xs
    | otherwise =onlyEven xs

onlyOdd []=[]
onlyOdd (x:xs)
    |x `mod` 2==1=x:onlyOdd xs
    |otherwise =onlyOdd xs

onlyUpper ""=""
onlyUpper (x:xs)
    |a>64 && a<91 = [x]++onlyUpper xs
    |otherwise =onlyUpper xs
    where a=fromEnum(x)

filter' :: (a->Bool)->[a]->[a]
filter' p []=[]
filter' p (x:xs)
    |p x==True=x:filter' p xs
    |otherwise= filter' p xs