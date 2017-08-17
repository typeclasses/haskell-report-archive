-- Standard list functions

module PreludeList  where

infixl 9  !!
infix  5  \\
infixr 5  ++
infix  4 `elem`, `notElem`

-- head and tail extract the first element and remaining elements,
-- respectively, of a list, which must be non-empty.  last and init
-- are the dual functions working from the end of a finite list,
-- rather than the beginning.

head			:: [a] -> a
head (x:_)		=  x
head []			=  error "head{PreludeList}: head []"

last			:: [a] -> a
last [x]		=  x
last (_:xs)		=  last xs
last []			=  error "last{PreludeList}: last []"

tail			:: [a] -> [a]
tail (_:xs)		=  xs
tail []			=  error "tail{PreludeList}: tail []"

init			:: [a] -> [a]
init [x]		=  []
init (x:xs)		=  x : init xs
init []			=  error "init{PreludeList}: init []"

-- null determines if a list is empty.
null			:: [a] -> Bool
null []			=  True
null (_:_)		=  False

-- list concatenation (right-associative)
(++)			:: [a] -> [a] -> [a]
xs ++ ys		=  foldr (:) ys xs

-- list difference (non-associative).  In the result of xs \\ ys,
-- the first occurrence of each element of ys in turn (if any)
-- has been removed from xs.  Thus, (xs ++ ys) \\ xs == ys.
(\\)			:: (Eq a) => [a] -> [a] -> [a]
(\\)			=  foldl del
			   where [] `del` _	    = []
				 (x:xs) `del` y
					| x == y    = xs
					| otherwise = x : xs `del` y

-- length returns the length of a finite list as an Int; it is an instance
-- of the more general genericLength, the result type of which may be
-- any kind of number.

genericLength		:: (Num a) => [b] -> a
genericLength		=  foldl (\n _ -> n+1) 0

length			:: [a] -> Int
length			=  genericLength

-- Array index (subscript) operator, 0-origin
(!!)			:: (Integral a) => [b] -> a -> b
(x:_)  !! 0		=  x
(_:xs) !! (n+1)		=  xs !! n
(_:_)  !! _		=  error "(!!){PreludeList}: negative index"
[]     !! (_+1)		=  error "(!!){PreludeList}: index too large"

-- map f xs applies f to each element of xs; i.e., map f xs == [f x | x <- xs].
map			:: (a -> b) -> [a] -> [b]
map f []		=  []
map f (x:xs)		=  f x : map f xs

-- filter, applied to a predicate and a list, returns the list of those
-- elements that satisfy the predicate; i.e.,
-- filter p xs == [x | x <- xs, p x].
filter			:: (a -> Bool) -> [a] -> [a]
filter p		=  foldr (\x xs -> if p x then x:xs else xs) []
 
-- partition takes a predicate and a list and returns a pair of lists:
-- those elements of the argument list that do and do not satisfy the
-- predicate, respectively; i.e.,
-- partition p xs == (filter p xs, filter (not . p) xs).
partition		:: (a -> Bool) -> [a] -> ([a],[a])
partition p		=  foldr select ([],[])
			   where select x (ts,fs) | p x	      = (x:ts,fs)
						  | otherwise = (ts,x:fs)

-- foldl, applied to a binary operator, a starting value (typically the
-- left-identity of the operator), and a list, reduces the list using
-- the binary operator, from left to right:
--	foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
-- foldl1 is a variant that has no starting value argument, and  thus must
-- be applied to non-empty lists.  scanl is similar to foldl, but returns
-- a list of successive reduced values from the left:
--	scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
-- Note that  last (scanl f z xs) == foldl f z xs.  scanl1 is similar,
-- again without the starting element:
--	scanl1 f [x1, x2, ...] = [x1, x1 `f` x2, ...]

foldl			:: (a -> b -> a) -> a -> [b] -> a
foldl f z []		=  z
foldl f z (x:xs)	=  foldl f (f z x) xs

foldl1			:: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs)		=  foldl f x xs

scanl			:: (a -> b -> a) -> a -> [b] -> [a]
scanl f q xs		=  q : (case xs of
				[]   -> []
				x:xs -> scanl f (f q x) xs)

scanl1			:: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs)		=  scanl f x xs

-- foldr, foldr1, scanr, and scanr1 are the right-to-left duals of the
-- above functions.

foldr			:: (a -> b -> b) -> b -> [a] -> b
foldr f z []		=  z
foldr f z (x:xs)	=  f x (foldr f z xs)

foldr1			:: (a -> a -> a) -> [a] -> a
foldr1 f [x]		=  x
foldr1 f (x:xs)		=  f x (foldr1 f xs)

scanr			:: (a -> b -> b) -> b -> [a] -> [b]
scanr f q0 []		=  [q0]
scanr f q0 (x:xs)	=  f x q : qs
			   where qs@(q:_) = scanr f q0 xs 

scanr1			:: (a -> a -> a) -> [a] -> [a]
scanr1 f  [x]		=  [x]
scanr1 f  (x:xs)	=  f x q : qs
			   where qs@(q:_) = scanr1 f xs 

-- iterate f x returns an infinite list of repeated applications of f to x:
-- iterate f x == [x, f x, f (f x), ...]
iterate			:: (a -> a) -> a -> [a]
iterate f x		=  x : iterate f (f x)

-- repeat x is an infinite list, with x the value of every element.
repeat			:: a -> [a]
repeat x		=  xs where xs = x:xs

-- cycle ties a finite list into a circular one, or equivalently,
-- the infinite repetition of the original list.  It is the identity
-- on infinite lists.

cycle			:: [a] -> [a]
cycle xs		=  xs' where xs' = xs ++ xs'

-- take n, applied to a list xs, returns the prefix of xs of length n,
-- or xs itself if n > length xs.  drop n xs returns the suffix of xs
-- after the first n elements, or [] if n > length xs.  splitAt n xs
-- is equivalent to (take n xs, drop n xs).

take			:: (Integral a) => a -> [b] -> [b]
take  0     _		=  []
take  _     []		=  []
take (n+1) (x:xs)	=  x : take n xs

drop			:: (Integral a) => a -> [b] -> [b]
drop  0     xs		=  xs
drop  _     []		=  []
drop (n+1) (_:xs)	=  drop n xs

splitAt			:: (Integral a) => a -> [b] -> ([b],[b])
splitAt  0     xs	=  ([],xs)
splitAt  _     []	=  ([],[])
splitAt (n+1) (x:xs)	=  (x:xs',xs'') where (xs',xs'') = splitAt n xs

-- takeWhile, applied to a predicate p and a list xs, returns the longest
-- prefix (possibly empty) of xs of elements that satisfy p.  dropWhile p xs
-- returns the remaining suffix.  Span p xs is equivalent to
-- (takeWhile p xs, dropWhile p xs), while break p uses the negation of p.

takeWhile		:: (a -> Bool) -> [a] -> [a]
takeWhile p []		=  []
takeWhile p (x:xs) 
            | p x       =  x : takeWhile p xs
            | otherwise =  []

dropWhile		:: (a -> Bool) -> [a] -> [a]
dropWhile p []		=  []
dropWhile p xs@(x:xs')
	    | p x       =  dropWhile p xs'
	    | otherwise =  xs

span, break		:: (a -> Bool) -> [a] -> ([a],[a])
span p []		=  ([],[])
span p xs@(x:xs')
	   | p x	=  (x:ys,zs) where (ys,zs) = span p xs'
	   | otherwise	=  ([],xs)
break p			=  span (not . p)

-- lines breaks a string up into a list of strings at newline characters.
-- The resulting strings do not contain newlines.  Similary, words
-- breaks a string up into a list of words, which were delimited by
-- white space.  unlines and unwords are the inverse operations.
-- unlines joins lines with terminating newlines, and unwords joins
-- words with separating spaces.

lines			:: String -> [String]
lines ""		=  []
lines s 		=  l : (if null s' then [] else lines (tail s'))
			   where (l, s') = break ((==) '\n') s

words			:: String -> [String]
words s			=  case dropWhile isSpace s of
				"" -> []
				s' -> w : words s''
				      where (w, s'') = break isSpace s'

unlines			:: [String] -> String
unlines ls		=  concat (map (\l -> l ++ "\n") ls)

unwords			:: [String] -> String
unwords []		= ""
unwords ws		= foldr1 (\w s -> w ++ ' ':s) ws

-- nub (meaning "essence") removes duplicate elements from its list argument.
nub			:: (Eq a) => [a] -> [a]
nub []			=  []
nub (x:xs)		=  x : nub (filter (/= x) xs)

-- reverse xs returns the elements of xs in reverse order.  xs must be finite.
reverse			:: [a] -> [a]
reverse			=  foldl (flip (:)) []

-- and returns the conjunction of a Boolean list.  For the result to be
-- True, the list must be finite; False, however, results from a False
-- value at a finite index of a finite or infinite list.  or is the
-- disjunctive dual of and.
and, or			:: [Bool] -> Bool
and			=  foldr (&&) True
or			=  foldr (||) False

-- Applied to a predicate and a list, any determines if any element
-- of the list satisfies the predicate.  Similarly, for all.
any, all		:: (a -> Bool) -> [a] -> Bool
any p			=  or . map p
all p			=  and . map p

-- elem is the list membership predicate, usually written in infix form,
-- e.g., x `elem` xs.  notElem is the negation.
elem, notElem		:: (Eq a) => a -> [a] -> Bool
elem			=  any . (==)
notElem			=  all . (/=)

-- sum and product compute the sum or product of a finite list of numbers.
sum, product		:: (Num a) => [a] -> a
sum			=  foldl (+) 0	
product			=  foldl (*) 1

-- sums and products give a list of running sums or products from
-- a list of numbers.  For example,  sums [1,2,3] == [0,1,3,6].
sums, products		:: (Num a) => [a] -> [a]
sums			=  scanl (+) 0
products		=  scanl (*) 1

-- maximum and minimum return the maximum or minimum value from a list,
-- which must be non-empty, finite, and of an ordered type.
maximum, minimum	:: (Ord a) => [a] -> a
maximum			=  foldl1 max
minimum			=  foldl1 min

-- concat, applied to a list of lists, returns their flattened concatenation.
concat			:: [[a]] -> [a]
concat			=  foldr (++) []

-- transpose, applied to a list of lists, returns that list with the
-- "rows" and "columns" interchanged.  The input need not be rectangular
-- (a list of equal-length lists) to be completely transposable, but can
-- be "triangular":  Each successive component list must be not longer
-- than the previous one; any elements outside of the "triangular"
-- transposable region are lost.  The input can be infinite in either
-- dimension or both.
transpose		:: [[a]] -> [[a]]
transpose		=  foldr 
			     (\xs xss -> zipWith (:) xs (xss ++ repeat []))
			     []

-- zip takes two lists and returns a list of corresponding pairs.  If one
-- input list is short, excess elements of the longer list are discarded.
-- zip3 takes three lists and returns a list of triples, etc.  Versions
-- of zip producing up to septuplets are defined here.

zip			:: [a] -> [b] -> [(a,b)]
zip			=  zipWith (\a b -> (a,b))

zip3			:: [a] -> [b] -> [c] -> [(a,b,c)]
zip3			=  zipWith3 (\a b c -> (a,b,c))

zip4			:: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
zip4			=  zipWith4 (\a b c d -> (a,b,c,d))

zip5			:: [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e)]
zip5			=  zipWith5 (\a b c d e -> (a,b,c,d,e))

zip6			:: [a] -> [b] -> [c] -> [d] -> [e] -> [f]
			   -> [(a,b,c,d,e,f)]
zip6			=  zipWith6 (\a b c d e f -> (a,b,c,d,e,f))

zip7			:: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g]
			   -> [(a,b,c,d,e,f,g)]
zip7			=  zipWith7 (\a b c d e f g -> (a,b,c,d,e,f,g))

-- The zipWith family generalises the zip family by zipping with the
-- function given as the first argument, instead of a tupling function.
-- For example, zipWith (+) is applied to two lists to produce the list
-- of corresponding sums.

zipWith			:: (a->b->c) -> [a]->[b]->[c]
zipWith z (a:as) (b:bs)	=  z a b : zipWith z as bs
zipWith _ _ _		=  []

zipWith3		:: (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith3 z (a:as) (b:bs) (c:cs)
			=  z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _	=  []

zipWith4		:: (a->b->c->d->e) -> [a]->[b]->[c]->[d]->[e]
zipWith4 z (a:as) (b:bs) (c:cs) (d:ds)
			=  z a b c d : zipWith4 z as bs cs ds
zipWith4 _ _ _ _ _	=  []

zipWith5		:: (a->b->c->d->e->f)
			   -> [a]->[b]->[c]->[d]->[e]->[f]
zipWith5 z (a:as) (b:bs) (c:cs) (d:ds) (e:es)
			=  z a b c d e : zipWith5 z as bs cs ds es
zipWith5 _ _ _ _ _ _	=  []

zipWith6		:: (a->b->c->d->e->f->g)
			   -> [a]->[b]->[c]->[d]->[e]->[f]->[g]
zipWith6 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs)
			=  z a b c d e f : zipWith6 z as bs cs ds es fs
zipWith6 _ _ _ _ _ _ _	=  []

zipWith7		:: (a->b->c->d->e->f->g->h)
			   -> [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]
zipWith7 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs)
		   =  z a b c d e f g : zipWith7 z as bs cs ds es fs gs
zipWith7 _ _ _ _ _ _ _ _ =  []
