-- Standard list functions

module PreludeList  where

infixl 9  !!
infixl 3  \\
infixr 3  ++
infix 2  `in`

head			:: [a] -> a
head (x:_)		=  x

last			:: [a] -> a
last [x]		=  x
last (_:xs)		=  last xs

tail			:: [a] -> [a]
tail (_:xs)		=  xs

init			:: [a] -> [a]
init [x]		=  []
init (x:xs)		=  x : init xs

null			:: [a] -> Bool
null []			=  True
null (_:_)		=  False

(++)			:: [a] -> [a] -> [a]
[] ++ ys		=  ys
(x:xs) ++ ys		=  x : (xs ++ ys)

length			:: (Integral a) => [b] -> a
length			=  foldl (\n _ -> n+1) 0

(!!)			:: (Integral a) => [b] -> a -> b
(x:_)  !! 0		=  x
(_:xs) !! (n+1)		=  xs !! n

map			:: (a -> b) -> [a] -> [b]
map f []		=  []
map f (x:xs)		=  f x : map f xs

filter			:: (a -> Bool) -> [a] -> [a]
filter p xs		=  [x | x <- xs, p x]

foldr			:: (a -> b -> b) -> b -> [a] -> b
foldr f z []		=  z
foldr f z (x:xs)	=  f x (foldr f z xs)

foldl			:: (a -> b -> a) -> a -> [b] -> a
foldl f z []		=  z
foldl f z (x:xs)	=  foldl f (f z x) xs

foldr1			:: (a -> a -> a) -> [a] -> a
foldr1 f [x]		=  x
foldr1 f (x:xs)		=  f x (foldr1 f xs)

foldl1			:: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs)		=  foldl f x xs

scan			:: (a -> b -> a) -> a -> [b] -> [a]
scan f q xs		=  q : case xs of
				[]   -> []
				x:xs -> scan f (f q x) xs

iterate			:: (a -> a) -> a -> [a]
iterate f x		=  x : iterate f (f x)

repeat			:: a -> [a]
repeat x		=  xs where xs = x:xs

cycle			:: [a] -> [a]
cycle xs		=  xs' where xs' = xs ++ xs'

take			:: (Integral a) => a -> [b] -> [b]
take  _     []		=  []
take  0     _		=  []
take (n+1) (x:xs)	=  x : take n xs

drop			:: (Integral a) => a  -> [b] -> [b]
drop  _     []		=  []
drop  0     xs		=  xs
drop (n+1) (_:xs)	=  drop n xs

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
span p xs		=  (takeWhile p xs, dropWhile p xs)
break p			=  span (not . p)

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
unlines ls		= concat (map (\l -> l ++ "\n") ls)

unwords			:: [String] -> String
unwords []		= ""
unwords [w]		= w
unwords (w:ws)		= w ++ concat (map ((:) ' ') ws)

in			:: (Eq a) => a -> [a] -> Bool
x `in` []		=  False
x `in` (y:ys)		=  x == y || x `in` ys

(\\)			:: (Eq a) => [a] -> [a] -> [a]
xs \\ ys		=  foldr remove ys xs

remove			:: (Eq a) => a -> [a] -> [a]
remove x		=  filter ((/=) x)

nub			:: (Eq a) => [a] -> [a]
nub []			=  []
nub (x:xs)		=  x : nub (remove x xs)

partition		:: (a -> Bool) -> [a] -> ([a],[a])
partition p xs		=  (filter p xs, filter (not . p) xs)

reverse			:: [a] -> [a]
reverse			=  foldl (\xs x -> x:xs) []

and, or			:: [Bool] -> Bool
and			=  foldr (&&) True
or			=  foldr (||) False

sum, product		:: (Num a) => [a] -> a
sum			=  foldl (+) 0
product			=  foldl (*) 1

maximum, minimum	:: (Ord a) => [a] -> a
maximum			=  foldl1 max
minimum			=  foldl1 min

concat			:: [[a]] -> [a]
concat			=  foldr (++) []

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
