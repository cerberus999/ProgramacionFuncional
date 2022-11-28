



--[x | x <- [1...], x<5]

filter' g xs = [x|x<-xs,g x]

map' f xs = [f x|x<-xs]

concat' xs = [x|x<-xs,x<-x]

length' xs = [x|x<-xs] 

prodCartesiano xs ys = [(x,y)|x<-xs,y<-ys]

exist xs elem = 
 if (exists /= [])
 then True
 else False
 where exists = [x|x<-xs,x==elem]

intersection xs ys = [x| x<-xs, y<-ys, x==y]

delElem xs eli = [x|x<-xs,x/=eli]





--Ej15
getDivisores n = [x|x<-[1..n], (mod n x) == 0]

esPrimo n =
 if length (getDivisores n) == 2 
 then True
 else False 



diagPrin mss = [ (mss !!i)!!i | i<-[0..ultimaFila]]
  where
    ultimaFila= (length mss) -1

diagSec mss = [(mss!!i)!!(tam-i) | i<-[0..tam]]
 where
 tam = (length mss) - 1


diagPrin2 mss = snd(foldr f a mss)
 where
  a=(ultimaFila,[])
  f fs (pos,rs)=(pos-1 ,(fs!!pos):rs)
  ultimaFila=length mss -1

{-
f[1,2,3,4] (f [1,2,3,4] (f [1,2,3,4] (f[1,2,3,4]] [])))
f[1,2,3,4] (f [1,2,3,4] (f [1,2,3,4] [4]] ))
f[1,2,3,4] (f [1,2,3,4] ([3,4]] ))
f[1,2,3,4] [2,3,4]
[1,2,3,4]

f[1,2,3,4] (f [1,2,3,4] (f [1,2,3,4] (f[1,2,3,4]] (3,[]))))
f[1,2,3,4] (f [1,2,3,4] (f [1,2,3,4] (2,[4]) ))
f[1,2,3,4] (f [1,2,3,4] (1,[3,4]] ))
f[1,2,3,4] (0,[2,3,4])
(-1,[1,2,3,4])
-}

--ej11

reversa n = [n-x|x<-[0..n]]
