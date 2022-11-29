fecMay:: (Int,Int,Int)->(Int,Int,Int)->(Int,Int,Int)
fecMay f1@(d1,m1,a1) f2@(d2,m2,a2) 
 |a1>a2=f1 
 |a2>a1=f2 
 |m1>m2=f1 
 |m2>m1=f2 
 |d1>d2=f1 
 |d2>d1=f2 
 |otherwise=f1

fecMay4 f1 f2 f3 f4 = fecMay (fecMay f1 f2) (fecMay f3 f4)