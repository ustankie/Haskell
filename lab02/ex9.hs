import GHC.Base (minInt, maxInt)
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
isSorted xs=( (loop2 True (maxInt) xs)||(loop True (minInt) xs) )
    where
        loop acc  big []=acc
        loop acc  big (x:xs)=loop (x>=big && acc) x xs 
        loop2 acc2 small []=acc2
        loop2 acc2 small (x1:xs1)=loop2 (x1<=small && acc2) x1 xs1

reverse' :: [a] -> [a]
reverse' []=[]
reverse' (x:xs)=reverse' xs ++[x]

zip' :: [a] -> [b] ->[(a,b)]
zip' [] []=[]
zip' xs []=[]
zip' [] ys =[]
zip' (x:xs) (y:ys)= (x,y):zip' xs ys


unzip' :: [(a,b)]->([a], [b] )
unzip' []=([],[])
unzip' ((x,y):xs)=(x:fst(unzip' xs),y:snd(unzip' xs) )

zip3' :: [a]->[b]->[c]->[(a,b,c)]
zip3' xs ys []=[]
zip3' xs [] zs=[]
zip3' [] ys zs=[]
zip3' (x:xs) (y:ys) (z:zs)=(x,y,z):zip3' xs ys zs

subList :: Eq a=> [a]->[a]->Bool
subList [] ys= True
subList xs []=False
subList (x:xs) (y:ys)
    |x==y = subList xs ys
    |otherwise =subList (x:xs) ys


mSort :: Ord a => [a]->[a]
mSort []=[]
mSort [x]=[x]
mSort xs=merge' (mSort leftPart) (mSort rightPart)
    where
        merge' [] ys=ys
        merge' xs []=xs
        merge' (x:xs) (y:ys)
            |x<y =x:merge' xs (y:ys)
            |otherwise=y:merge' (x:xs) ys 
        leftPart =take (((length xs) `div` 2)) xs
        rightPart =drop (((length xs) `div` 2)) xs

        

iSort :: Ord a=> [a]->[a]
iSort []=[]
iSort [x]=[x]
iSort (x:xs)=insert' (iSort xs)
    where 
        insert' [] =[x]
        insert' (y:ys) |x<y =x:y:ys
                       |otherwise =y:insert' ys
