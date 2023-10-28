fst2Eq :: Eq a=> [a]->Bool
fst2Eq (x:y:_) | x==y =True
fst2Eq _ =False

fstDivScnd :: Integral a =>  [a]->Bool
fstDivScnd (x:y:_) | y `mod` x==0 =True
fstDivScnd _                      =False



fstDiv3 :: Integral a => [a]->Bool
fstDiv3 (x:y:z:_)| z `mod` x==0 =True
fstDiv3 _                       =False