not' :: Bool->Bool
not' b=case b of
    True -> False
    False ->True

absInt n=
    case (n>=0) of 
        True -> n
        _   ->(-n)

isItTheAnswer :: String -> Bool
isItTheAnswer s=
    case (s) of
        "Love" -> True
        _   ->False

or' (a,b)=case (a,b) of
    (False, False)->False
    _   ->True

nand' (a,b)=case (a,b) of
    (True,True)->False
    otherwise ->True