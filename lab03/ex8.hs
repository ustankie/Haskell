doubleElems []=[]
doubleElems (x:xs)=2*x:doubleElems xs

map' :: (a->b)->[a]->[b]
map' f []=[]
map' f (x:xs)=f x:map' f xs

sumWith g []=0
sumWith g (x:xs)=g x+ sumWith g xs

prodWith g []=1
prodWith g (x:xs)=g x* prodWith g xs

sumWith' :: Num a=> (a->a)->[a]->a
sumWith' =go 0
    where 
        go acc g []=acc
        go acc g (x:xs)=go (g x + acc) g xs

prodWith' :: Num a=> (a->a)->[a]->a
prodWith' =go 1
    where 
        go acc g []=acc
        go acc g (x:xs)=go (g x * acc) g xs

foldr' :: (a->b->b)->b->[a]->b
foldr' f z []=z
foldr' f z (x:xs)=foldr' f (f x z) xs

foldl' :: (b->a->b)->b->[a]->b
foldl' f z []=z
foldl' f z (x:xs)=foldl' f (f z x) xs

