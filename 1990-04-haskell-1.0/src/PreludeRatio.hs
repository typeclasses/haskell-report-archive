-- Standard functions on rational numbers

module	PreludeRatio (
    Ratio, Rational, (%), numerator, denominator,
    approximants, partialQuotients ) where

infix  7  %, :%

prec = 7

data  (Integral a)	=> Ratio a = a :% a  deriving (Eq, Binary)
type  Rational		=  Ratio Integer

(%)			:: (Integral a) => a -> a -> Ratio a
numerator, denominator	:: (Integral a) => Ratio a -> a
approximants		:: (Integral a) => a -> a -> [Ratio a]
partialQuotients	:: (Integral a) => a -> a -> [a]


reduce x y		=  (x `div` d) :% (y `div` d)
			   where d = gcd x y


x % y			=  reduce (x * signum y) (abs y)

numerator (x:%y)	=  x

denominator (x:%y)	=  y

approximants p q 	=  zipWith (:%) ps qs
			   where
			   ps = gen unit (unit*a)
			   qs = gen 0 1
			   unit = signum p * signum q
			   a:as = partialQuotients (abs p) (abs q)
			   gen x x' = xs
				      where
				      xs = x' : zipWith3 next as (x:xs) xs
				      next a x x' = x'*a + x

partialQuotients p q	=  a : (if r==0 then [] else partialQuotients q r)
			   where (a,r) = divRem p q


instance  (Integral a)	=> Ord (Ratio a)  where
    (x:%y) <= (x':%y')	=  x * y' <= x' * y

instance  (Integral a)	=> Num (Ratio a)  where
    (x:%y) + (x':%y')	=  ((x*m) `div` y + (x'*m) `div` y') :% m
			   where m = lcm y y'
    (x:%y) * (x':%y')	=  reduce (x * x') (y * y')
    negate (x:%y)	=  (-x) :% y
    abs (x:%y)		=  abs x :% y
    signum (x:%y)	=  signum x :% 1
    fromInteger x	=  fromInteger x :% 1

instance  (Integral a)	=> Real (Ratio a)  where
    toRational (x:%y)	=  toInteger x :% toInteger y

instance  (Integral a)	=> Fractional (Ratio a)  where
    (x:%y) / (x':%y')	=  (x*y') % (y*x')
    fromRational (x:%y) =  fromInteger x :% fromInteger y

instance  (Integral a)	=> RealFrac (Ratio a)  where
    properFraction (x:%y) = (toInteger q, r:%y)
			    where (q,r) = divRem x y

    approxRational x@(p:%q) eps =
	case withinEps of
	    r:r':_ | denominator r == denominator r' -> r'
	    r:_					     -> r
	where withinEps = dropWhile (\r -> abs (r-x) > eps)
    	    	    	    	    (approximants p q)

instance  (Integral a, Text a) => Text (Ratio a)  where
    readsPrec p  =  readParen (p > prec)
			      (\r -> [(x%y,u) | (x,s)   <- reads r,
					        ("%",t) <- [lex s],
						(y,u)   <- reads t ])

    showsPrec p (x:%y)	=  showParen (p > prec)
    	    	    	       (shows x . showString " % " . shows y)
