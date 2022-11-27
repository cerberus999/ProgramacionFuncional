



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

--ej11
matriz xss = [x|x<-y,[y|y<-xss]]

