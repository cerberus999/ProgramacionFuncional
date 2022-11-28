
factorial 0 = 1
factorial n = n * factorial (n-1)

sumatoria n | n==0 = n
            | n>0  = n + sumatoria (n-1)


filter' f [] = []
filter' f (x:xs) = if f x then x:ys else ys
 where ys = filter' f xs


cociente dvdd dvs = cociente' dvdd dvs 0 

cociente' dvdd dvs coc = 
 if dvs > dvdd 
  then 0 
  else (cociente' (dvdd-dvs) dvs coc+1)

residuo dvdd dvs = 
 if dvdd < dvs 
  then dvdd
  else residuo (dvdd-dvs) dvs

{-restarListas [] (y:ys) = []
restarListas (x:xs) (y:ys)
 |x/=[] = restarListas xs (y:ys)
 |y==[] = x:zs
 |x/=y  = restarListas (x:xs) ys
 |x==y  = restarListas xs ys 
 where zs = []
-}

rotarDerecha (x:xs) 0 = []
rotarDerecha (x:xs) n =
 rotarDerecha (xs++[x]) n-1

-------------------------------------------------------------
--Ej 1
listasIguales [] (y:ys) = False
listasIguales (x:xs) [] = False
listasIguales [] []     = True
listasIguales (x:xs) (y:ys) 
 |x==y = True && listasIguales xs ys
 |x/=y = False

--Ej 4
enOrden (x:xs) f = 
 if xs == []
 then True
 else if f x (head xs)  
  then True && enOrden xs f
  else False

--Ej 5
listaPares [] = []
listaPares (n:ns) = 
 if (mod n) 2 == 0 
  then n:listaPares ns
  else listaPares ns 

--Ej 6 
soloListasPares [[]] = [[]]
soloListasPares [] = []
soloListasPares (xs:xss) =
 if (mod (length xs) 2) == 0
  then xs:soloListasPares xss 
  else soloListasPares xss

--Ej 7
eliminarParesMatrix [] = []
eliminarParesMatrix (xs:xss) =
 if ys /= [] 
 then ys:eliminarParesMatrix xss
 else eliminarParesMatrix xss
 where ys = eliminarParesLista xs
 
eliminarParesLista [] = []
eliminarParesLista (x:xs) =
 if (mod x 2) == 0
 then eliminarParesLista xs 
 else x:eliminarParesLista xs

--Ej8
getPreLast [] = []
getPreLast (xs:xss) = 
 if tam >= 2 
  then (!!) xs (tam - 2):getPreLast xss 
  else getPreLast xss 
  where tam = length xs

--Ej9 
getDivisors n = 
 getDivisors' n n 

getDivisors' n 0 = []
getDivisors' divdd divsr =
 if mod divdd divsr == 0
 then divsr:getDivisors' divdd (divsr-1) 
 else getDivisors' divdd (divsr-1)

 --Ej 10 
buscar (x:xs) y = 
 buscar' (x:xs) y 0

buscar' [] y r = error "Not found"
buscar' (x:xs) y r = 
 if x == y 
 then r
 else buscar' xs y (r+1)
 

--Ej 11
--Los elementos deben estar ordenados de menor a mayor
busquedaBinaria [] n = -1
busquedaBinaria lst n = 
 busquedaBinaria' lst n 0 ((length lst) -1)

busquedaBinaria' lst n minn maxx = 
 if minn == maxx && lst !! pm /= n 
 then -1
 else if lst !! pm == n 
  then pm 
  else if (lst !! pm) > n
   then busquedaBinaria' lst n minn (pm-1)
   else busquedaBinaria' lst n (pm+1) maxx
 where pm = div (minn + maxx) 2

{---Ej12
ordenSD [] = []
ordenSD lst = 
 ordenSD' lst (head lst)

ordenSD' lst aux = 
-} 