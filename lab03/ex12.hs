prodPrices p=case p of
    "A"->100
    "B"->500
    "C"->1000
    _->error "Unknown product"

products=["A","B","C"]

discStr1 p
    |price>999=0.3*price
    |otherwise=0.1*price
    where price=prodPrices p

discStr2 p=0.2*prodPrices p

totalDiscount discStr=
    foldl1 (+) .
    map discStr .
    filter (\p->prodPrices p>499)