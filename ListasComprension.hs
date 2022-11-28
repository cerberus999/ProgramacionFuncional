



--[x | x <- [1...], x<5]
--Ej1
filter' g xs = [x|x<-xs,g x]

--Ej2
map' f xs = [f x|x<-xs]

--Ej3
concat' xs = [x|x<-xs,x<-x]

--Ej4 Aun no terminado
length' xs = [x|x<-xs] 

--Ej5
encrypt::[Char] -> [Char]
encrypt xs = [changeVocals x|x<-xs]

changeVocals c
 |c=='a' || c=='A'= '1'
 |c=='e' || c=='E' = '2'
 |c=='i' || c=='I' = '3'
 |c=='o' || c=='O' = '4'
 |c=='u' || c=='U' = '5'
 |otherwise = c

--Ej6
prodCartesiano xs ys = [(x,y)|x<-xs,y<-ys]

--Ej7
exist xs elem = 
 if (exists /= [])
 then True
 else False
 where exists = [x|x<-xs,x==elem]

--Ej8
intersection xs ys = [x| x<-xs, y<-ys, x==y]

--Ej9
delElem xs eli = [x|x<-xs,x/=eli]

--Ej16
zipWith' f lst1 lst2 = [f x y|x<-lst1,y<-lst2]