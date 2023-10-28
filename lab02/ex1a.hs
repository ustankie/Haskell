myFun x=2*x

add2T :: Num a => (a,a) -> a
add2T (x,y)=x+y

add2C :: Num a => a->(a -> a)
add2C x y=x+y

add3T :: Num a=> (a,a,a) -> a
add3T (x,y,z)=x+y+z

add3C :: Num a => a->(a -> (a->a))
add3C x y z=x+y+z

-- curry2 :: ((a,b)->c) -> a -> b -> curry2
-- curry2 f x y=\(x,y)

fiveToPower_ :: Integer -> Integer
fiveToPower_ = (5^)

_ToPower5 :: Num a => a->a
_ToPower5 =(^5)

subtrNFrom5 :: Num a => a->a
subtrNFrom5 = (-) 5

subtr5From_:: Num a => a->a
subtr5From_ =flip (-) 5

flip' :: (a->b->c)->b->a->c
flip' f x y=f y x


flip3 :: (a->b->c->d)->c->b->a->d
flip3 f x y z=f z y x

dPower ::( Num a,Integral b,Integral c) =>a->b->c->a
dPower x y z=(^) ( x^y) z

