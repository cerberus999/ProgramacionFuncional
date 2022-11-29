import Distribution.Simple.Setup (falseArg)
--Ejercicios Fold  r  l 

--Ejercicio1
productoria xs = 
 foldr (*) 1 xs


--Ejercicio2
mapeado:: (a->b)->[a]->[b]
mapeado f [] = []
mapeado f (x:xs) = foldr (\y ys -> (f y):ys) [] (x:xs)


--Ejercicio3
filter' f [] = []
filter' f (x:xs) = foldr (\y ys -> if(f y) then y:ys else ys) [] (x:xs)


--Ejercicio4
length' [] = 0
length' (x:xs) = foldr (\y ys-> 1+ys) 0 (x:xs) 

--plusArray [] [] = []
--plusArray (x:xs) (y:ys) = foldr (\z zs -> (x:xs) [0..n ]:(y:ys)) [] (x:xs)

--Falta(++) Ej 5

volcarArray [] = 0
volcarArray (x:xs) = foldr (\s1 s2 -> ((s2*10) + s1)) 0 (x:xs)

volcarArrayX [] = []
volcarArrayX (x:xs) = foldr (\s1 s2 -> ([s1]++s2)) [] (x:xs)

--esAscendente (x:xs) = foldr (\y1 y2 -> if y2 /= [] then y1:y1-y2!!0:[y1] else [y1] ) [] (x:xs)

--Ejercicio6
reverse' [] = []
reverse' (x:xs) = foldr (\y1 y2 -> y2++[y1]) [] (x:xs)

--Ejercicio7
concat' [] = []
concat' (x:xs) = foldr (\y ys -> y++ys) [] (x:xs)

--Ejercicio 8
takeWhile' f [] = []
takeWhile' f (x:xs) = (\y1 y2 -> if f y1 then y1++y2 else []) [] (x:xs)

--Falta Ej9 Lista de digitos => retorna el numero formado al juntalos 

--Ejercicio10
minVal [] = -1
minVal (x:xs) = foldr (\y1 y2 -> if y2 >= y1 then y1 else y2) (last (x:xs)) (x:xs)

--Ejercicio11
replaceMin [] = []
replaceMin (x:xs) = foldr (\y1 y2 -> y2 !! 0:y2) [minVal (x:xs)] (xs)

