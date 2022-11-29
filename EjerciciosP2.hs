--Ejercicio2

--Ej1
mayorDe2::Int->Int->Int
mayorDe2 a b = 
 if a>b 
  then a 
  else b

--Ej2
mayorDe3::Int->Int->Int->Int
mayorDe3 a b c = 
 if a>b && a>c 
  then a 
 else 
  if b>a && b>c
   then b
   else c

--Ej3
mayorDe4::Int->Int->Int->Int->Int
mayorDe4 a b c d = 
 if a>b && a>c && a>d 
 then a 
 else 
  if b>a && b>c && b>d
  then b
  else 
   if c>a && c>b && c>d
   then c
   else d


--Ej4
examenes::Double->Double->Int->Int->String
examenes pp sp ef si =
 if ((pp+sp)/2)>50 || ef>50 || si>50
 then "Aprobado"
 else
  if (pp+sp)/2==1
  then "Abandono"
  else "Reprobado"

--Ej5
--fecMay::()->()->()
fecMay f1@(d1,m1,a1) f2@(d2,m2,a2) =
 if a1>a2
 then f1  
 else if a2>a1
  then f2
  else if m1>m2
   then f1
   else if m2>m1
    then f2
    else if d1>d2
    then f1
    else f2

