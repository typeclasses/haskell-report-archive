-- Standard types, classes, and instances

module PreludeCore (
    Eq((=), (/=)),
    Ord((<), (<=), (>=), (>), max, min),
    Num((+), (-), (*), negate, abs, signum, fromInteger),
    Integral(divRem, div, rem, mod, even, odd, toInteger),
    Fractional((/), fromRational),
    Floating(pi, exp, log, sqrt, (**), logBase,
	     sin, cos, tan, asin, acos, atan,
	     sinh, cosh, tanh, asinh, acosh, atanh),
    Real(toRational),
    RealFrac(properFraction, approxRational),
    RealFloat(floatRadix, floatDigits, floatRange,
	      encodeFloat, decodeFloat, exponent, significand, scaleFloat),
    Ix(range, index, inRange),
    Enum(enumFrom, enumFromThen, enumFromTo, enumFromThenTo),
    Text(readsPrec, showsPrec, readList, showList),
    Binary(readBin, showBin),
--  List type: [_]((:), [])
--  Tuple types: (_,_), (_,_,_), etc.
--  Trivial type: () 
    Bool(True, False),
    Char, Int, Integer, Float, Double, Bin,
    Ratio, Complex((:+)), Assoc((:=)), Array,
    String, Rational )  where

import PreludeBuiltin
import PreludeText(Text(readsPrec, showsPrec, readList, showList))
import PreludeRatio(Ratio, Rational)
import PreludeComplex
import PreludeArray(Assoc(:=), Array)
import PreludeIO(Name, Request, Response, IOError,
		 Dialogue, SuccCont, StrCont, BinCont, FailCont)

infixr 8  **
infixl 7  *
infix  7  /, `div`, `rem`, `mod`
infixl 6  +, -
infixr 3  :
infix  2  ==, /=, <, <=, >=, >


-- Equality and Ordered classes

class  Eq a  where
    (==), (/=)		:: a -> a -> Bool

    x /= y		=  not (x == y)

class  (Eq a) => Ord a  where
    (<), (<=), (>=), (>):: a -> a -> Bool
    max, min		:: a -> a -> Bool

    x <	 y		=  x <= y && x /= y
    x >= y		=  y <= x
    x >	 y		=  y <	x
    max x y | x >= y	=  x
	    | y >= x	=  y
    min x y | x <= y	=  x
	    | y <= x	=  y


-- Numeric classes

class  (Eq a) => Num a  where
    (+), (-), (*)	:: a -> a -> a
    negate		:: a -> a
    abs, signum		:: a -> a
    fromInteger		:: Integer -> a

    x - y		=  x + negate y

class  (Num a, Ord a) => Real a	where
    toRational		::  a -> Rational

class  (Real a) => Integral a  where
    div, rem, mod	:: a -> a -> a
    divRem		:: a -> a -> (a,a)
    even, odd		:: a -> Bool
    toInteger		:: a -> Integer

    x `div` y		=  q  where (q,r) = divRem x y
    x `rem` y		=  r  where (q,r) = divRem x y
    x `mod` y 		=  if signum x == - (signum y) then r + y else r
			   where r = x `rem` y
    even x		=  x `rem` 2 == 0
    odd			=  not . even

class  (Num a) => Fractional a  where
    (/)			:: a -> a -> a
    fromRational	:: Rational -> a

class  (Fractional a) => Floating a  where
    pi			:: a
    exp, log, sqrt	:: a -> a
    (**), logBase	:: a -> a -> a
    sin, cos, tan	:: a -> a
    asin, acos, atan	:: a -> a
    sinh, cosh, tanh	:: a -> a
    asinh, acosh, atanh :: a -> a

    x ** y		=  exp (log x * y)
    logBase x y		=  log y / log x
    sqrt x		=  x ** 0.5
    tan  x		=  sin  x / cos  x
    tanh x		=  sinh x / cosh x

class  (Real a, Fractional a) => RealFrac a  where
    properFraction	:: a -> (Integer,a)
    approxRational	:: a -> a -> Rational

class  (RealFrac a, Floating a) => RealFloat a  where
    floatRadix		:: a -> Integer
    floatDigits		:: a -> Int
    floatRange		:: a -> (Int,Int)
    decodeFloat		:: a -> (Integer,Int)
    encodeFloat		:: Integer -> Int -> a
    exponent		:: a -> Int
    significand		:: a -> a
    scaleFloat		:: Int -> a -> a

    exponent x		=  if m == 0 then 0 else n + floatDigits x
			   where (m,n) = decodeFloat x

    significand x	=  encodeFloat m (- (floatDigits x))
			   where (m,_) = decodeFloat x

    scaleFloat k x	=  encodeFloat m (n+k)
			   where (m,n) = decodeFloat x


-- Index and Enumeration classes

class  (Ord a) => Ix a  where
    range		:: (a,a) -> [a]
    index		:: (a,a) -> a -> Int
    inRange		:: (a,a) -> a -> Bool

class  (Ix a) => Enum a	where
    enumFrom		:: a -> [a]		-- [n..]
    enumFromThen	:: a -> a -> [a]	-- [n,n'..]
    enumFromTo		:: a -> a -> [a]	-- [n..m]
    enumFromThenTo	:: a -> a -> a -> [a]	-- [n,n'..m]

    enumFromTo n m	=  takeWhile ((>=) m) (enumFrom n)
    enumFromThenTo n n' m
			=  takeWhile ((if n' >= n then (>=) else (<=)) m)
				     (enumFromThen n n')

-- Binary class

class  Binary a  where
    readBin		:: Bin -> (a,Bin)
    showBin		:: a -> Bin -> Bin


-- Boolean type

data  Bool  =  False | True


-- Character type

instance  Eq Char  where
    c == c'		=  ord c ==  ord c'

instance  Ord Char  where
    c <= c'		=  ord c <= ord c'

instance  Ix Char  where
    range (c,c')	=  [c..c']
    index (c,c') ci	=  ord ci - ord c
    inRange (c,c') ci	=  ord c <= i && i <= ord c'
			   where i = ord ci

instance  Enum Char  where
    enumFrom c		=  map chr [ord c ..]
    enumFromThen c c'	=  map chr [ord c, ord c' ..]

type  String = [Char]


-- Standard Integral types

instance  Eq Int  where
    (==)		=  primEqInt

instance  Eq Integer  where
    (==)		=  primEqInteger

instance  Ord Int  where
    (<=)		=  primLeInt

instance  Ord Integer  where
    (<=)		=  primLeInteger

instance  Num Int  where
    (+)			=  primPlusInt
    negate		=  primNegInt
    (*)			=  primMulInt
    abs			=  absReal
    signum		=  signumReal
    fromInteger		=  primIntegerToInt

instance  Num Integer  where
    (+)			=  primPlusInteger
    negate		=  primNegInteger
    (*)			=  primMulInteger
    abs			=  absReal
    signum		=  signumReal
    fromInteger x	=  x
    
absReal x    | x >= 0	 =  x
	     | otherwise =  - x

signumReal x | x == 0	 =  0
   	     | x > 0	 =  1
	     | otherwise = -1

instance  Real Int  where
    toRational x	=  toInteger x % 1

instance  Real Integer	where
    toRational x	=  x % 1

instance  Integral Int	where
    divRem		=  primDivRemInt
    toInteger		=  primIntToInteger

instance  Integral Integer  where
    divRem		=  primDivRemInteger
    toInteger x		=  x

instance  Ix Int  where
    range (m,n)		=  [m..n]
    index (m,n) i	=  i - m
    inRange (m,n) i	=  m <= i && i <= n

instance  Ix Integer  where
    range (m,n)		=  [m..n]
    index (m,n) i	=  fromInteger (i - m)
    inRange (m,n) i	=  m <= i && i <= n

instance  Enum Int  where
    enumFrom n		=  enumFromBy n 1
    enumFromThen n m	=  enumFromBy n (m - n)

instance  Enum Integer  where
    enumFrom n		=  enumFromBy n 1
    enumFromThen n m	=  enumFromBy n (m - n)

enumFromBy n k		=  n : enumFromBy (n+k) k


-- Standard Floating types

instance  Eq Float  where
    (==)		=  primEqFloat

instance  Eq Double  where
    (==)		=  primEqDouble

instance  Ord Float  where
    (<=)		=  primLeFloat

instance  Ord Double  where
    (<=)		=  primLeDouble

instance  Num Float  where
    (+)			=  primPlusFloat
    negate		=  primNegFloat
    (*)			=  primMulFloat
    abs			=  absReal
    signum		=  signumReal
    fromInteger n	=  encodeFloat n 0

instance  Num Double  where
    (+)			=  primPlusDouble
    negate		=  primNegDouble
    (*)			=  primMulDouble
    abs			=  absReal
    signum		=  signumReal
    fromInteger n	=  encodeFloat n 0

instance  Real Float  where
    toRational		=  floatingToRational

instance  Real Double  where
    toRational		=  floatingToRational

floatingToRational x	=  (m%1)*(b%1)^^n
			   where (m,n) = decodeFloat x
				 b     = floatRadix  x

instance  Fractional Float  where
    (/)			=  primDivFloat
    fromRational	=  rationalToFloating

instance  Fractional Double  where
    (/)			=  primDivDouble
    fromRational	=  rationalToFloating

rationalToFloating x	=  fromInteger (numerator x)
				/ fromInteger (denominator x)

instance  Floating Float  where
    pi			=  primPiFloat
    exp			=  primExpFloat
    log			=  primLogFloat
    sqrt		=  primSqrtFloat
    sin			=  primSinFloat
    cos			=  primCosFloat
    tan			=  primTanFloat
    asin		=  primAsinFloat
    acos		=  primAcosFloat
    atan		=  primAtanFloat
    sinh		=  primSinhFloat
    cosh		=  primCoshFloat
    tanh		=  primTanhFloat
    asinh		=  primAsinhFloat
    acosh		=  primAcoshFloat
    atanh		=  primAtanhFloat

instance  Floating Double  where
    pi			=  primPiDouble
    exp			=  primExpDouble
    log			=  primLogDouble
    sqrt		=  primSqrtDouble
    sin			=  primSinDouble
    cos			=  primCosDouble
    tan			=  primTanDouble
    asin		=  primAsinDouble
    acos		=  primAcosDouble
    atan		=  primAtanDouble
    sinh		=  primSinhDouble
    cosh		=  primCoshDouble
    tanh		=  primTanhDouble
    asinh		=  primAsinhDouble
    acosh		=  primAcoshDouble
    atanh		=  primAtanhDouble


instance  RealFrac Float  where
    properFraction	=  floatProperFraction
    approxRational	=  floatApproxRational

instance  RealFrac Double  where
    properFraction	=  floatProperFraction
    approxRational	=  floatApproxRational

floatProperFraction x	=  if n >= 0
			      then (m * b^n, 0)
			      else (m', fromInteger k / fromInteger d)
			   where (m,n)	= decodeFloat x
				 b	= floatRadix x
				 (m',k) = divRem m d
				 d	= b^(-n)
floatApproxRational x eps =
    case withinEps of
	r:r':_ | denominator r == denominator r' -> r'
	r:_				         -> r
    where withinEps = dropWhile (\r -> abs (fromRational r - x) > eps)
				(approximants p q)
	  (p,q)     = if n < 0 then (m, b^(-n)) else (m*b^n, 1)
	  (m,n)     = decodeFloat x
	  b	    = toInteger (floatRadix x)

instance  RealFloat Float  where
    floatRadix _	=  primFloatRadix
    floatDigits _	=  primFloatDigits
    floatRange _	=  (primFloatMinExp,primFloatMaxExp)
    decodeFloat		=  primDecodeFloat
    encodeFloat		=  primEncodeFloat

instance  RealFloat Double  where
    floatRadix _	=  primDoubleRadix
    floatDigits	_	=  primDoubleDigits
    floatRange _	=  (primDoubleMinExp,primDoubleMaxExp)
    decodeFloat		=  primDecodeDouble
    encodeFloat		=  primEncodeDouble

instance  Ix Float  where
    range (x,y)		=  [x..y]
    index (x,y) i	=  floor (i - x)
    inRange (x,y) i	=  x <= i && i <= y

instance  Ix Double  where
    range (x,y)		=  [x..y]
    index (x,y) i	=  floor (i - x)
    inRange (x,y) i	=  x <= i && i <= y

instance  Enum Float  where
    enumFrom x		=  enumFromBy x 1
    enumFromThen x y	=  enumFromBy x (y - x)

instance  Enum Double  where
    enumFrom x		=  enumFromBy x 1
    enumFromThen x y	=  enumFromBy x (y - x)
