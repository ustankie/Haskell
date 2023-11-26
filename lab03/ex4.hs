import Data.List (sort)
funcList :: [Double->Double]
funcList =[\x->(sin x)/x,\x->log x+sqrt x +1,\x->(exp 1) **x]

evalFuncList :: a->[a->b]->[b]
evalFuncList x []=[]
evalFuncList x (f:fs)=f x : evalFuncList x fs

displEqs :: (Double->Double,Double->Double)
displEqs = (\t->4*t^2+2*t,\t->3*t^2)

sortDesc :: Ord a=>[a]->[a]
sortDesc xs= (reverse . sort) xs

sortDesc1 :: Ord a=>[a]->[a]
sortDesc1 = (reverse . sort) 