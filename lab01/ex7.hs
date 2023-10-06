not' :: Bool -> Bool
not' True=False
not' False=True

isItTheAnswer :: String -> Bool
isItTheAnswer "Love" = True
isItTheAnswer _    = False

or' :: (Bool,Bool) -> Bool
or' (False,False)=False
or' (x,y)=True

and' :: (Bool, Bool) -> Bool
and' (True,True)=True
and' (x,y)=False

nand' :: (Bool, Bool) -> Bool
nand' (True,True)=False
nand' (x,y)=True

xor' :: (Bool, Bool) -> Bool
xor' (True,False)=True
xor' (False,True)=True
xor' (x,y)=False