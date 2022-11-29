--Ejercicio 3
--Ej1
elMayor2::Int->Int->Int
elMayor2 n1 n2 |n1>=n2=n1 |n2>n1=n2

elMayorComb4::Int->Int->Int->Int->Int
elMayorComb4 n1 n2 n3 n4 = 
 elMayor2 (elMayor2 n1 n2) (elMayor2 n3 n4)

elMayorDist4::Int->Int->Int->Int->Int
elMayorDist4 n1 n2 n3 n4
 |n1>=n2 && n1>=n3 && n1>=n4 = n1
 |n2>=n1 && n2>=n3 && n2>=n4 = n2
 |n3>=n1 && n3>=n2 && n3>=n4 = n3
 |n4>=n1 && n4>=n2 && n4>=n3 = n4

 --Ej2
notaApr::Int->String
notaApr n |n>50 = "Aprobado" |n<=50 = "Reprobado"

--Ej3
notaDif::Int->String
notaDif n
 |n>=90 && n<=100 = "Excelente"
 |n>=70 && n<90   = "Bien"
 |n>50  && n<70   = "Regular"
 |n>=0  && n<=50  = "Mal"
 |otherwise       = "Incorrect Value"

--Ej4
notasDif::Double->Double->Int->Int->String
notasDif pp sp ef si
 |prom>50 || ef>50 || si>50 = "Aprobado"
 |otherwise = "Reprobado" 
 where prom = (pp + sp) / 2

--Ej5 incompleto
elMayor16::[Int]->Int
elMayor16 (x:xs)
 |x>elMayor16 xs = x
 |xs == [x] = x
 |otherwise = elMayor16 xs

--ej6
quebMay1::Double->Double->Bool
quebMay1 num den 
 |queb > 1  = True 
 |queb <= 1 =  False
 where queb = (num / den)

--ej7
fecMay::(Int,Int,Int)->(Int,Int,Int)->(Int,Int,Int)
fecMay f1@(d1,m1,a1) f2@(d2,m2,a2)
 |d1>d2 = f1
 |d2>d1 = f2
 |m1>m2 = f1
 |m2>m1 = f2
 |a1>a2 = f1
 |a2>a1 = f2
 |otherwise = f1

--ej8
fecDiffanios::(Int,Int,Int)->(Int,Int,Int)->Int
fecDiffanios (d1,m1,a1) (d2,m2,a2)
 |diff1>0 = diff1
 |diff2>0 = diff2
 |otherwise = diff1
 where 
  diff1 = (a1-a2)
  diff2 = (a2-a1)


--ej9
fecDiffmeses::(Int,Int,Int)->(Int,Int,Int)->Int
fecDiffmeses (d1,m1,a1) (d2,m2,a2)
 |diff1>0 = diff1
 |diff2>0 = diff2
 |otherwise = diff1
 where 
  diff1 = (m1-m2)
  diff2 = (m2-m1)


--ej10
fecDiffdias::(Int,Int,Int)->(Int,Int,Int)->Int
fecDiffdias (d1,m1,a1) (d2,m2,a2)
 |diff1>0 = diff1
 |diff2>0 = diff2
 |otherwise = diff1
 where 
  diff1 = (d1-d2)
  diff2 = (d2-d1)


--ej11
fecDiffFec::(Int,Int,Int)->(Int,Int,Int)->(Int,Int,Int)
fecDiffFec f1@(d1,m1,a1) f2@(d2,m2,a2) =
 (fecDiffdias f1 f2,fecDiffmeses f1 f2,fecDiffanios f1 f2)

--ej12
incInstante::((Int,Int,Int),(Int,Int,Int))->((Int,Int,Int),(Int,Int,Int))
incInstante ((dia,mes,anio),(hora,minu,segu)) = 
 ((dia,mes,anio),(hora,minu,segu+1))





f12:: (ty->(trz->tr))->ty->(tw->trz)->tw->tr
f12 x y z w = ((x y) (z w))

f:: Int->Int->tz->Int
f x y z = x*2+y

--22--
f99::(Int->(Int->Int))->Int->Int->Int
f99 x y z = r1 + r2
 where   
  r1 = x 5 y
  r2 = x y z



--index de listas--
ej2 xss y = (head (head xss)) y


--Definir una funcion que reciba una lista de INTs y devuelva los pares
pares xs = (filter (even)) xs 

--lista de listas -> devolver lista de las longitudes
listLongit xss = map length xss

--recibe lista de listas -> solo donde la longitud sea par
listasPares xss = filter (even) (listLongit xss)

-- Listas de listas INT -> borrar numeros pares de las listas
borrarParesMat2 xss = map (filter (odd)) xss

--lista de listas -> lista formada por los penultimos elementos de la lista
--penultimos xss = map (take ((map length xss) - 1) xss

penultimos xss = (map ((subtract) 2) (map length xss))

func x y@(w,z) s 
 |s (\a -> a*3) = x
 |w ((&& True) z) = y

func2 x y@(w,z) s
 |s (\a -> a*3) = x 
 |w ((&& True) z) = y 

func3 x y@(z,w) s
 |s ((&& True) w) = y 

func4 x y z w
 |y = False
 |x (\z -> z*3.2) = w