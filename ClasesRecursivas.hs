



{-
suma::Natural->Natural->Natural
suma n 0 = n
suma n m = 1 + (suma n (m - 1))
-}

estaOrd::[a]->Bool
estaOrd [] = True
estaOrd [x]=True
estaOrd (x:y:xs) = x<=y && estaOrd(y:xs)

data Lista a = Vacia | Add a (Lista a)

estaO::Lista Int->Bool
estaO Vacia =True
estaO (Add x Vacia) = True
estaO (Add x (Add y ys)) = 
 if x<=y
 then estaO (Add y ys)
 else False 

selDir::Num a => [a]->[a]
selDir ds = sel [] ds

sel os [] = os
sel os ds = sel (os++menDes) (borrar ds)
 where 
  menDes = getMenor ds

getMenor [x] = x 
getMenor (x:y:xs) = 
 if x<y 
 then getMenor (x:xs)
 else getMenor (y:xs)

borrar e [] = []
borrar e (x:xs) 
 | e == x = xs
 | otherwise = x:(borrar e xs)

qs [] = []
qs [x] = [x]
qs (d:ds)=(qs menores)++[d]++(qs mayores)
 where 
  menores = filter (<d) ds
  mayores = filter (>=d) ds 

