module  Arrays ( Array, Assoc(:=), array, listArray, (!), bounds, indices, 
		 elems, assocs, accumArray, (//), accum, amap, ixmap
	       ) where

-- This module specifies the semantics of arrays only: it is not
-- intended as an efficient implementation.

infixl 9  !
infix  5  :=
infixl 4  //

data  Assoc a b =  a := b
data  (Ix a)    => Array a b = MkArray (a,a) (a -> b) deriving ()

array		:: (Ix a) => (a,a) -> [Assoc a b] -> Array a b
listArray	:: (Ix a) => (a,a) -> [b] -> Array a b
(!)		:: (Ix a) => Array a b -> a -> b
bounds		:: (Ix a) => Array a b -> (a,a)
indices		:: (Ix a) => Array a b -> [a]
elems		:: (Ix a) => Array a b -> [b]
assocs		:: (Ix a) => Array a b -> [Assoc a b]
accumArray	:: (Ix a) => (b -> c -> b) -> b -> (a,a) -> [Assoc a c]
			     -> Array a b
(//)		:: (Ix a) => Array a b -> Assoc a b -> Array a b
accum		:: (Ix a) => (b -> c -> b) -> Array a b -> [Assoc a c]
			     -> Array a b
amap		:: (Ix a) => (b -> c) -> Array a b -> Array a c
ixmap		:: (Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c
			     -> Array a c

array b ivs =
    if and [inRange b i | i:=_ <- ivs] then
    	MkArray b (\j-> v where [v] = [v | i := v <- ivs, i == j])

listArray b vs = array b (zipWith (:=) (range b) vs)

(!) (MkArray _ f)     = f

bounds (MkArray b _)  = b

indices               = range . bounds

elems a               = [a!i | i <- indices a]

assocs a              = [i := a!i | i <- indices a]

a // iv@(i := v)      = array b (iv : [j := a!j | j <- range b, i /= j])
    	                where b = bounds a

accum f               = foldl (\a (i := v)-> a // i := f (a!i) v)

accumArray f z b      = accum f (array b [i := z | i <- range b])

amap f a              = array b [i := f (a!i) | i <- range b]
                        where b = bounds a

ixmap b f a           = array b [i := a ! f i | i <- range b]

instance  (Ix a, Eq b)  => Eq (Array a b)  where
    a == a'  	        =  assocs a == assocs a'

instance  (Ix a, Ord b) => Ord (Array a b)  where
    a <=  a'  	    	=  assocs a <=  assocs a'

instance  (Ix a, Text a, Text b) => Text (Array a b)  where
    showsPrec p a = showParen (p > 9) (
		    showString "array " .
		    shows (bounds a) . showChar ' ' .
		    shows (assocs a)                  )

    readsPrec p = readParen (p > 9)
	   (\r -> [(array b as, u) | ("array",s) <- [lex r],
				     (b,t)       <- reads s,
				     (as,u)      <- reads t   ]
		  ++
		  [(listArray b xs, u) | ("listArray",s) <- [lex r],
					 (b,t)           <- reads s,
					 (xs,u)          <- reads t ])

instance  (Ix a, Binary a, Binary b) => Binary (Array a b)  where
    showBin a = showBin (bounds a) . showBin (elems a)

    readBin bin = (listArray b vs, bin'')
		 where (b,bin')   = readBin bin
		       (vs,bin'') = readBin bin'
