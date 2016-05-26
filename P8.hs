module Exp
	where

-- 8a.

exp :: Floating a => a -> a
data Exp = Constant Int -4
		| Variable string x
		| Minus (Minus Exp Exp) -x + 4
		| Division Exp Exp -x / 4
		| Power Exp Exp x(**-4)
		| Exponential Exp Exp exp(x ^ (- 4)) 
		| Log Exp Exp log (x - 4)
		| Tg Exp Exp tg(x - 4)
		| Ctg Exp Exp ctg (x - 4)
		| Arctg Exp Exp arctg (x - 4)
		deriving Show
		
derive :: (Fractional a) => a -> (a -> a) -> (a -> a)
derive h f x  = (f (x+h) - f x) / h 
derive h f -4 = (f (-4 + h)	- f -4) / h
derive h f -x + 4 = (f (-x + 4 + h)) -f -x + 4) / h
derive h f -x/4 = (f -x/4 + h) - f -x/4) / h
derive h f x(**-4) = (f x(**-4)) + h) - f - x(**-4)) / h
derive h f exp(x ^ (- 4)) = (f exp(x ^ (- 4)) + h) - f - exp(x ^ (- 4))) / h
derive h f log(x - 4) = (f log(x - 4) + h) - f log (x - 4)) / h
derive h f tg (x - 4) = (f tg(x - 4) + h) - f tg(x - 4)) / h
derive h f ctg (x - 4) = (f ct(x - 4) + h) - f ctg(x - 4)) / h
derive h f arctg(x - 4) = (f arctg(x - 4) + h) - f arctg(x - 4)) / h

scriereExp :: Floating a => a -> a
scriereExp Minus (Minus Exp Exp) - x
		| -x + 4 == True
		| otherwise == False
scriereExp Divison Exp Exp x /
		| -x / 4 == True
		| otherwise == False
scriereExp Power Exp Exp x(**)
		| x(**-4) == True
		| otherwise == False 
scriereExp Exponential Exp exp exp(x^)
		| exp(x ^ (-4)) == True
		| otherwise == False
scriereExp Log Exp Exp log x
		| log(x - 4) == True
		| otherwise == False
scriereExp Exp Tg tg x
		| tg(x - 4) == True
		| otherwise == False
scriereExp Exp Ctg ctg x
		| ctg(x - 4) == True
		| otherwise == False
scriereExp Exp Arctg arctg x
		| arctg(x - 4) == True
		| otherwise == False

-- 8b.

integrare :: Exp -> Exp
integrare (x) = primitiva(x)  
integrare (x ** 2 - x + 6) = "2 ** x - 1"

-- 8c.
poly+to_str :: [a] => Exp -> Exp 
poly_to_str (a,b):
		return ''.join (['{:+d}a**{:d}'.format(a,x,b) 
		for b, a in enumerate(a,b)][::-1]) 
poly_to_str(3 * x + 5): 
		| 3 ** x + 5 == True
		| otherwise == 3 
