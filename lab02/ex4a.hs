import GHC.Base (BCO)
isPalindrome :: [Char]->Bool
isPalindrome s|reverse s ==s =True
            | otherwise=False

getElemAtIdx ::  [a]-> Int->a
getElemAtIdx a b=head(drop b a)


capitalize :: [Char]->[Char]
capitalize s=if a>=97 && a<=122
            then toEnum(a-32):tail s
            else s
            where a=fromEnum (head s)

isPrime :: (Integral t)=> t-> Bool
isPrime n=[i|i<-[2..a],n `mod`i==0]==[]
            where a=(floor ( sqrt ( fromIntegral n)) )

primesFrom1To10000:: Int
primesFrom1To10000 =length [x| x<-[2..10000], [i|i<-[2..x-1],x `mod` i==0]==[]]

primes :: [Int]
primes=eratoSieve [2 ..]
    where 
        eratoSieve :: [Int]-> [Int]
        eratoSieve (p:xs)=p:eratoSieve[x|x<-xs,x `mod` p/=0]

isPrime2 :: Int->Bool
isPrime2 n=b primes
    where
        b :: [Int]->Bool
        b (p1:p)= p1==n || (head(p)<=n && b p)


howManyPrimes :: Int->Int
howManyPrimes n=length[x| x<-[1..n], isPrime2 x]


allEqual :: Eq a=> [a]->Bool
allEqual (a:xs)=(length[x|x<-xs,x==a])==(length xs )

