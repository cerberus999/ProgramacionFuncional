



data Natural = Cero | Suc Natural
 deriving Show

--def una f que reciba un numero entero y lo convierta a natural

toNatural:: Int -> Natural
toNatural n
 |n==0 = Cero
 |n>0  = Suc (toNatural (n-1))

toEntero::Natural -> Int
toEntero Cero = 0
toEntero (Suc n) = 1+(toEntero n)
