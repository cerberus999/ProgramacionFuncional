f1::Num a=> Bool->a->a->a
f1 x y z  =
 if x then y+10 else z

f2::Num a => a->a->Char->a
f2 x y z = 
 if z=='s' 
  then 2*x
  else y

f3::Num a => (a->Bool) -> (Char->Bool) -> (Bool->Bool) -> Bool
f3 x y z = (x 2) && (y 'a') && (z True)

f4::Num a => (a->Bool) -> p -> a -> a -> a
f4 = (\x -> \y -> \z -> \w -> if x 2 then z else w+10)

f5:: Num a => (p1->a,p1) -> (p2->a,p2) -> a
f5 = (\(x,y) -> \(z,w) -> (x y)+(z w))

f6:: t -> t
f6 x = x

f7:: (t1->tr)->t1->tr
f7 x y = x y 

f8::(ry->r)->(tz->ry)->tz->r 
f8 x y z = x (y z)

f9::(t1->(t2->r))->t1->t2->r
f9 x y z = (x y) z

f10::(t3->r)->(t2->t3)->(t->t2)->t->r
f10 x y z w = (x (y (z w)))

f11::(t1->t2->t3->r)->t1->t2->t3->r
f11 x y z w = ((x y) z) w

f12::(t1->t3->r)->t1->(t2->t3)->t2->r
f12 x y z w = ((x y)(z w))

f13::Num a =>a->a->f->a
f13 x y z = x*2 + y

f16::(t1->t2->r)->t1->t2->r
f16 x y z = x y z

f17::Bool->Bool->Bool->Bool
f17 x y z 
 |x = y
 |y = z

f18::(q->q->r)->(p->q)->p->r
f18 x y z = x (y z) (y z)

a19::(tx->ty->r)->(tx,ty)->r
a19 a (x,y) = a x y

u20::((tx,ty)->r)->tx->ty->r
u20 f x y = f(x,y)

f21::Num a=>p->a->(a->a)->a
f21 x y z = s + y
 where s = z y

f22::Num a=> (a->a->a) -> a -> a -> a
f22 x y z = r1 +r2
 where r1 = x 5 y
       r2 = x y z 

f23::((p,q)->r)->p->q->r
f23 f = g 
 where g x y = f (x,y)

f24::(tx->ty->r)->(tx,ty)->r
f24 f = g 
 where g (x,y) = f x y

f25::((x,y,z)->r)->x->y->z->r
f25 f = g 
 where g x y z = f (x,y,z)

f26::Bool->Bool->(Bool->Bool)->Bool->f->Bool
f26 e x y z w = if x && (y x) then z else w z
 where w a | a = e
           | otherwise = z

------------------------------------------------------------

curry1::((tx,ty)->r)->tx->ty->r
curry1 f x y = f(x,y)

curry2::((tx,ty)->r)->tx->ty->r
curry2 f = g 
 where g x y = f(x,y)

uncurry1::(tx->ty->r)->(tx,ty)->r
uncurry1 f (x,y) = f x y

uncurry2::(tx->ty->tr)->(tx,ty)->tr
uncurry2 f = g 
 where g(x,y) = f x y

suma1::Num a=> a->a->a
suma1 x y = x + y

suma2::Num a =>  (a,a)->a
suma2 (x,y) = x + y

--curry1 suma1 ::Num tx, ty => tx->ty->(tx,ty)->
