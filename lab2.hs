
--Question 1
--EX1 monext f add 0 [3,4,7] = (3*3 + 2) + (4*3 + 2) + (7*3 + 2) = 48
--EX2 monext f times 1 [2,4,1] = 
monext (f, bin, e) list

--Question 2
faspec1 'a' 0 = 1
faspec1 'b' 0 = 0
faspec1 'a' 1 = 2
faspec1 'b' 1 = 1
faspec1 'a' 2 = 3
faspec1 'b' 2 = 2
faspec1 'a' 3 = 0 
faspec1 'b' 3 = 3

comp f g x = g (f x)
e x = x
--comp f e x = e (f x) = f x identity

fa spec string(val:rest) num = comp (spec val num) (fa spec rest num)

fa1 = fa faspec1
fa1 "asdasdasd" 0 =     

