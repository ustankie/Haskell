isSortedAsc :: Ord a=>[a]->Bool
isSortedAsc xs=all (\(x,y)->x<y) (zip xs (tail xs))

everySecond :: [t]->[t]
everySecond xs=map fst(filter (\(x, y)->even y) (zip xs [0..]))

concat' :: [[a]]->[a]
concat' []=[]
concat' (x:xs)=x++concat' xs


