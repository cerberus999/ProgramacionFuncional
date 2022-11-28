--Ejercicio 1
areaCuad:: Int->Int
areaCuad l = l*l 

--Ejercicio 4
primMay:: Int->Int->Bool
primMay n1 n2 = if n1>n2 then True else False

--Ejercicio 5
esMultiplo2:: Int->Bool
esMultiplo2 n = if((mod n 2) == 0) then True else False

--Ejercicio 6
esMultiplo2y3:: Int->Bool
esMultiplo2y3 n = if((mod n 2) == 0) && ((mod n 3) == 0) then True else False

--Ejercicio 7
potencia3 n = n^3

--Ejercicio 8.1
potencia4 n = n^4

--Ejercicio 8.2
potencia8 n = n^8

--Ejercicio 8.3
potencia10 n = n^10

--Ejercicio 8.4
potencia32 n = n^32
