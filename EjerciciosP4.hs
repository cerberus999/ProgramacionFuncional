--Ejercicio 4

--Ej1
sigVoc::Char->Char
sigVoc c =
 case c of 
 'a' -> 'e'
 'e' -> 'i'
 'i' -> 'o'
 'o' -> 'u'
 'u' -> 'a'

--Ej2
numToLiteral::Int->String
numToLiteral n = 
 case n of
  0 -> "Cero"
  1 -> "Uno"
  2 -> "Dos"
  3 -> "Tres"
  4 -> "Cuatro"
  5 -> "Cinco"
  6 -> "Seis"
  7 -> "Siete"
  8 -> "Ocho"
  9 -> "Nueve"
  _ -> "?"

--Ej3
funcAnd::Int->Int->Int
funcAnd v1 v2 =
 case v1==1 && v2==1 of
    True -> 1
    False -> 0

--Ej4
funcOr::Int->Int->Int
funcOr v1 v2 =
 case v1==0 && v2==0 of
    True -> 0
    False -> 1

--Ej5
funcXor::Int->Int->Int
funcXor v1 v2 =
 case v1/=v2 of
    True -> 1
    False -> 0


--Ej6
funcLog::String->Int->Int->Int
funcLog name v1 v2 =
 case name of
  "and" -> funcAnd v1 v2
  "or"  -> funcOr  v1 v2
  "xor" -> funcXor v1 v2

--Ej7
--Ej8 
numLiteral2Dig:: Int->String
numLiteral2Dig n =  
 case segDig of
  1 -> "Dieci"
  2 -> "Veinti"
  3 -> "Treinta y "
  4 -> "Cuarenta y "
  5 -> "Cincuenta y "
  6 -> "Sesenta y "
  7 -> "Setenta y "
  8 -> "Ochenta y"
  9 -> "Noventa y "
 ++
 numToLiteral priDig
 where 
  priDig = mod n 10
  segDig = mod (div n 10) 10

--Ej9
elMenor::Int->Int->Int
elMenor n1 n2 = 
 case n1 <= n2 of
  True  -> n1
  False -> n2

--Ej10
elMenor6::Int->Int->Int->Int->Int->Int->Int
elMenor6 n1 n2 n3 n4 n5 n6 =
 elMenor (elMenor n1 n2) (elMenor (elMenor n3 n4) (elMenor n5 n6))

--Ej11
sumatoria11 n1 n2 n3 =
 case suma<20 && suma>=10 of
 True  -> "Sumatoria mayor"
 False -> case suma<10  of
  True  -> "Sumatoria menor"
  False -> "Vacio"
 where 
  suma = n1 + n2 + n3

--Ej12
notas n1 n2 n3 =
 case promedio>=90 && promedio<=100 of 
  True  -> "Excelente"
  False -> case promedio>=70 && promedio<90 of
   True  -> "Bien"
   False -> case promedio>=51 && promedio<70 of
    True  -> "Regular"
    False -> case promedio<51 of
     True  -> "Mal"
 where 
 promedio = (n1+n2+n3)/3

 

f2::(ty->tz->tr)->ty->tz->tr
f2 x y z = x y z