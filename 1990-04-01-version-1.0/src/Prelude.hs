-- Standard value bindings

module Prelude (
    PreludeCore.., PreludeRatio.., PreludeList.., PreludeArray.., 
    PreludeText.., PreludeIO.., 
    nullBin, isNullBin,
    (&&), (||), not,
    ord, chr, 
    isAscii, isControl, isPrint, isSpace, 
    isUpper, isLower, isAlpha, isDigit, isAlphanum,
    toUpper, toLower,
    gcd, lcm, (^), (^^), 
    truncate, round, ceiling, floor, fromIntegral, fromRealFrac, atan2,
    realPart, imagPart, conjugate, mkPolar, cis, polar, magnitude, phase,
    fst, snd, (.), until, error, asTypeOf, otherwise ) where

import PreludeBuiltin
import PreludeCore
import PreludeList
import PreludeArray
import PreludeRatio
import PreludeText
import PreludeIO

infixr 9  .
infixr 8  ^, ^^
infixr 1  &&
infixr 0  ||


-- Binary functions

nullBin	    	    	:: Bin
nullBin	    	    	=  primNullBin

isNullBin    	    	:: Bin -> Bool
isNullBin    	    	=  primIsNullBin

-- Boolean functions

(&&), (||)		:: Bool -> Bool -> Bool
True  && x		=  x
False && x		=  False
True  || x		=  True
False || x		=  x

not			:: Bool -> Bool
not True		=  False
not False		=  True

otherwise		:: Bool
otherwise 		= True

-- Character functions

ord			:: Char -> Int
ord 			= primCharToInt

chr 			:: Int -> Char
chr 			= primIntToChar

isAscii, isControl, isPrint, isSpace		:: Char -> Bool
isUpper, isLower, isAlpha, isDigit, isAlphanum	:: Char -> Bool

isAscii c	 	=  ord c < 128
isControl c		=  c < ' ' || c == '\DEL'
isPrint c		=  c >= ' ' && c <= '~'
isSpace c		=  c == ' ' || c == '\t' || c == '\n' || 
			   c == '\r' || c == '\f' || c == '\v'
isUpper c		=  c >= 'A' && c <= 'Z'
isLower c		=  c >= 'a' && c <= 'z'
isAlpha c		=  isUpper c || isLower c
isDigit c		=  c >= '0' && c <= '9'
isAlphanum c		=  isAlpha c || isDigit c


toUpper, toLower	:: Char -> Char
toUpper c | isLower c	=  chr (ord c - ord 'a' + ord 'A')
	  | otherwise	=  c

toLower c | isUpper c	=  chr (ord c - ord 'A' + ord 'a')
	  | otherwise	=  c

-- Numeric functions

minInt, maxInt	:: Int
minInt		=  primMinInt
maxInt		=  primMaxInt

gcd		:: (Integral a) => a -> a-> a
gcd x 0		=  abs x
gcd 0 y		=  abs y
gcd x y		=  gcd' (abs x) (abs y)
		   where gcd' x y  =  if x == y then x
				      else gcd' y (x `rem` y)

lcm		:: (Integral a) => a -> a-> a
lcm _ 0		=  0
lcm 0 _		=  0
lcm x y		=  abs ((x `div` (gcd x y)) * y)

(^)		:: (Num a, Integral b) => a -> b -> a
x ^ 0		=  1
x ^ (n+1)	=  f x n x
		   where f _ 0 y = y
		         f x n y = g x n  where
			           g x n | even n  = g (x*x) (n`div`2)
				         | otherwise = f x (n-1) (x*y)

(^^)		:: (Fractional a, Integral b) => a -> b -> a
x ^^ n		=  if n >= 0 then x^n else 1/x^(-n)

truncate	:: (RealFrac a, Integral b) => a -> b
truncate x	=  fromInteger m  where (m,r) = properFraction x

round		:: (RealFrac a, Integral b) => a -> b
round x		=  fromInteger y
		   where y = case signum (abs r - 0.5) of
				-1 -> n
			 	0  -> if even n then n else m
				1  -> m
			 (n,r) = properFraction x
			 m     = if r < 0 then n - 1 else n + 1

ceiling		:: (RealFrac a, Integral b) => a -> b
ceiling x	=  fromInteger (if r > 0 then n + 1 else n)
		   where (n,r) = properFraction x

floor		:: (RealFrac a, Integral b) => a -> b
floor x		=  fromInteger (if r < 0 then n - 1 else n)
		   where (n,r) = properFraction x

fromIntegral	:: (Integral a, Num b) => a -> b
fromIntegral	=  fromInteger . toInteger

fromRealFrac	:: (RealFrac a, Fractional b) => a -> b
fromRealFrac	=  fromRational . toRational

atan2		:: (RealFloat a) => a -> a -> a
atan2 y x	=  case (signum y, signum x) of
			( 0, 1) ->  0
			( 1, 0) ->  pi/2
			( 0,-1) ->  pi
			(-1, 0) -> -pi/2
			( _, 1) ->  atan (y/x)
			( _,-1) ->  atan (y/x) + pi
			-- (0,0) is an error

realPart, imagPart :: (RealFloat a) => Complex a -> a
realPart (x:+y)	 =  x
imagPart (x:+y)	 =  y

conjugate	 :: (RealFloat a) => Complex a -> Complex a
conjugate (x:+y) =  x :+ (-y)

mkPolar		 :: (RealFloat a) => a -> a -> Complex a
mkPolar r theta	 =  r * sin theta :+ r * cos theta

cis		 :: (RealFloat a) => a -> Complex a
cis theta	 =  sin theta :+ cos theta

polar		 :: (RealFloat a) => Complex a -> (a,a)
polar z		 =  (magnitude z, phase z)

magnitude, phase :: (RealFloat a) => Complex a -> a
magnitude (x:+y) =  scaleFloat k
		     (sqrt ((scaleFloat mk x)^2 + (scaleFloat mk y)^2))
		    where k  = max (exponent x) (exponent y)
		          mk = - k

phase (x:+y)	 =  atan2 y x


-- Some standard functions

fst			:: (a,b) -> a
fst (x,y)		=  x

snd			:: (a,b) -> b
snd (x,y)		=  y

(.)			:: (b -> c) -> (a -> b) -> a -> c
(f . g) x		=  f (g x)

until			:: (a -> Bool) -> (a -> a) -> a -> a
until p f x | p x	=  x
	    | otherwise =  until p f (f x)

error			:: String -> a
error msg | False  	=  error msg

asTypeOf		:: a -> a -> a
x `asTypeOf` _ 		= x
