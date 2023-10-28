qSort :: Ord a => [a]->[a]
qSort []=[]
qSort (x:xs)=qSort (leftPart xs)++[x]++(rightPart xs)
    where 
        leftPart xs=[y|y<-xs,y<=x]
        rightPart xs=[y|y<-xs,y>x]

qSort2 :: Ord a => [a]->[a]
qSort2 []=[]
qSort2 (x:xs)=qSort2 (leftPart xs)++[x]++(rightPart xs)
    where 
        leftPart xs=filter (<=x) xs
        rightPart xs=filter (>x) xs

concat' :: [[a]]->[a]
concat' a=[ x |b<-a ,x<-b]

concat'' :: [[a]]->[a]
concat'' [[]]=[]
concat'' ([]:xss)=(concat'' xss)
concat'' ((x:xs):xss)=x:(concat'' (xs:xss))


isSorted :: [Int]->Bool
isSorted=loop True -Infinity
    where
        loop acc [] big=acc
        loop acc (x:xs) =loop (x>)