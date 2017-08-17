module	PreludeText (
	Text(readsPrec,showsPrec,readList,showList),
	ReadS, ShowS, reads, shows, show, read, lex,
	showChar, showString, readParen, showParen ) where

type  ReadS a = String -> [(a,String)]
type  ShowS   = String -> String

class  Text a  where
    readsPrec :: Int -> ReadS a
    showsPrec :: Int -> a -> ShowS
    readList  :: ReadS [a]
    showList  :: [a] -> ShowS

    readList    = readParen False
    	    	    	 (\r -> [pr | ("[",s) <- [lex r], pr <- readl s])
	          where readl s = [([],t) | ("]",t) <- [lex s]] ++
			          [(x:xs,v) | (x,t) <- reads s,
				    	      (",",u) <- [lex t],
					      (xs,v) <- readl u   ]
    showList xs = showChar '[' . showl xs
		  where showl []     = showChar ']'
		        showl (x:xs) = shows x . showChar ',' . showl xs

reads 	        :: (Text a) => ReadS a
reads 	    	=  readsPrec 0

shows 	    	:: (Text a) => a -> ShowS
shows 	    	=  showsPrec 0

read 	    	:: (Text a) => String -> a
read s 	    	=  x
    	    	   where [x] = [x | (x,t) <- reads s, ("","") <- [lex t]]

show 	    	:: (Text a) => a -> String
show x 	    	=  shows x ""

showChar    	:: Char -> ShowS
showChar    	=  (:)
showString  	:: String -> ShowS
showString  	=  (++)
showParen   	:: Bool -> ShowS -> ShowS
showParen b p 	=  if b then showChar '(' . p . showChar ')' else p

readParen   	:: Bool -> ReadS a -> ReadS a
readParen b g	=  if b then mandatory else optional
		   where optional r  = g r ++ mandatory r
			 mandatory r = [(x,u) | ("(",s) <- [lex r],
						(x,t)   <- optional s,
						(")",u) <- [lex t]    ]
lex 	    	:: String -> (String,String)
lex ""		= ("","")
lex ('-':'>':s)	= ("->",s)
lex ('-':s)	= ("-",s)
lex r@(c:s)	= 
	if      isSpace c	then lex (dropWhile isSpace s)
	else if isAlpha c	then span isIdChar r
	else if isSingleSym c	then ([c],s)
	else if isMultiSym c	then span isMultiSym r
	else if isDigit c	then lexNum r
	else if c == '\''	then ('\'' : ch ++ "'", u)
			    	where {(ch,t) = lexLitChar s; '\'':u = t}
	else if c == '"'	then ('"':str, t)
			    	where (str,t) = lexString s
	else error "bad character"
  where
	isIdChar c	   = isAlphanum c || c == '_' || c == '\''
	isSingleSym c	   = c `in` ",;()[]{}_"
	isMultiSym c	   = c `in` "!@#$%&*+-./<=>?\\^|~"

	lexNum r = (ds++f, t) where (ds,s) = span isDigit r
				    (f,t)  = lexFracExp s
	lexFracExp ('.':r) = ('.':ds++e, t)
				where (ds,s) = lexDigits r
				      (e, t) = lexExp s
	lexFracExp s       = ("",s)

	lexExp ('e':'-':r) = ("e-"++ds, s) where (ds,s) = lexDigits r
	lexExp ('e':r)     = ('e':ds, s)   where (ds,s) = lexDigits r
	lexExp s           = ("",s)

	lexDigits r@(d:_) | isDigit d = span isDigit r

	lexString ('"':s)  = ("\"", s)
	lexString s        = (ch++str, u)
			     where (ch,t)  = lexLitChar s
				   (str,u) = lexString t

lexLitChar  	:: String -> (String,String)
lexLitChar ('\\':s) = ('\\':esc, t)
		      where (esc,t) = lexEsc s
			    lexEsc (c:s) | c `in` "abfnrtv\\\"'&" = ([c],s)
			    lexEsc ('^':c:s) | isUpper c = (['^',c], s)
			    lexEsc ('N':'U':'L':s) = ("NUL", s)
			    lexEsc ('S':'O':'H':s) = ("SOH", s)
			    lexEsc ('S':'T':'X':s) = ("STX", s)
			    lexEsc ('E':'T':'X':s) = ("ETX", s)
			    lexEsc ('E':'O':'T':s) = ("EOT", s)
			    lexEsc ('E':'N':'Q':s) = ("ENQ", s)
			    lexEsc ('A':'C':'K':s) = ("ACK", s)
			    lexEsc ('B':'E':'L':s) = ("BEL", s)
			    lexEsc ('B':'S':s) = ("BS", s)
			    lexEsc ('H':'T':s) = ("HT", s)
			    lexEsc ('L':'F':s) = ("LF", s)
			    lexEsc ('V':'T':s) = ("VT", s)
			    lexEsc ('F':'F':s) = ("FF", s)
			    lexEsc ('C':'R':s) = ("CR", s)
			    lexEsc ('S':'O':s) = ("SO", s)
			    lexEsc ('S':'I':s) = ("SI", s)
			    lexEsc ('D':'L':'E':s) = ("DLE", s)
			    lexEsc ('D':'C':'1':s) = ("DC1", s)
			    lexEsc ('D':'C':'2':s) = ("DC2", s)
			    lexEsc ('D':'C':'3':s) = ("DC3", s)
			    lexEsc ('D':'C':'4':s) = ("DC4", s)
			    lexEsc ('N':'A':'K':s) = ("NAK", s)
			    lexEsc ('S':'Y':'N':s) = ("SYN", s)
			    lexEsc ('E':'T':'B':s) = ("ETB", s)
			    lexEsc ('C':'A':'N':s) = ("CAN", s)
			    lexEsc ('E':'M':s) = ("EM", s)
			    lexEsc ('S':'U':'B':s) = ("SUB", s)
			    lexEsc ('E':'S':'C':s) = ("ESC", s)
			    lexEsc ('F':'S':s) = ("FS", s)
			    lexEsc ('G':'S':s) = ("GS", s)
			    lexEsc ('R':'S':s) = ("RS", s)
			    lexEsc ('U':'S':s) = ("US", s)
			    lexEsc ('S':'P':s) = ("SP", s)
			    lexEsc ('D':'E':'L':s) = ("DEL", s)
			    lexEsc r@(d:s) | isDigit d = span isDigit r
			    lexEsc ('o':s) = ('o':os, t)
				     where (os,t) = nonempty
						    (\c -> c >= '0' &&
							   c <= '7' )
			    lexEsc ('x':s) = ('x':xs, t)
				     where (xs,t) = nonempty
						    (\c -> isDigit c ||
							   c >= 'A' &&
							   c <= 'F' )
			    lexEsc r@(c:s) | isSpace c = (sp++"\\", u)
						 where
						 (sp,t) = span isSpace s
						 ('\\',u) = t
							 
			    nonempty p r@(c:s) | p c = span p r
lexLitChar (c:s)    =  ([c],s)


-- Trivial type

instance  Text ()  where
    readsPrec p    = readParen False
    	    	    	    (\r -> [((),t) | ("(",s) <- [lex r],
					     (")",t) <- [lex s] ] )
    showsPrec p () = showString "()"


-- Character type

instance  Text Char  where
    readsPrec p      = readParen False
    	    	    	    (\r -> [(c,t) | ('\'':s,t)<-[lex r],
					    (c,_)     <-[readLitChar s]])

    showsPrec p '\'' = showString "'\\''"
    showsPrec p c    = showChar '\'' . showLitChar c . showChar '\''

    readList = readParen False (\r -> [(cs,t) | ('"':s, t) <- [lex r],
					        pr <- readl s])
	      where readl s = [("",t)   | '"':t  <- [s] ] ++
			      [(c:cs,u) | (c ,t) <- readLitChar s,
					  (cs,u) <- readl u	]

    showList cs = showChar '"' . showl cs
		 where showl ""        = showChar '"'
		       showl ('\'':cs) = showString "\\'" . showl cs
		       showl (c:cs)    = showLitChar c . showl cs

readLitChar 	:: ReadS Char
readLitChar s =	if ignore ch then readLitChar t else [(charVal ch, t)]
		where
		(ch,t) = lexLitChar s

		ignore "\\&" = True
		ignore ('\\':c:_) | isSpace c = True
		ignore _ = False

		charVal ('\\':esc) = escVal esc
		charVal [c]	  = c

		escVal "a" = '\a'
		escVal "b" = '\b'
		escVal "f" = '\f'
		escVal "n" = '\n'
		escVal "r" = '\r'
		escVal "t" = '\t'
		escVal "v" = '\v'
		escVal "\\" = '\\'
		escVal "\"" = '"'
		escVal "'" = '\''
		escVal ('^':[c]) = chr (ord c - 64)
		escVal "NUL" = '\NUL'
		escVal "SOH" = '\SOH'
		escVal "STX" = '\STX'
		escVal "ETX" = '\ETX'
		escVal "EOT" = '\EOT'
		escVal "ENQ" = '\ENQ'
		escVal "ACK" = '\ACK'
		escVal "BEL" = '\BEL'
		escVal "BS" = '\BS'
		escVal "HT" = '\HT'
		escVal "LF" = '\LF'
		escVal "VT" = '\VT'
		escVal "FF" = '\FF'
		escVal "CR" = '\CR'
		escVal "SO" = '\SO'
		escVal "SI" = '\SI'
		escVal "DLE" = '\DLE'
		escVal "DC1" = '\DC1'
		escVal "DC2" = '\DC2'
		escVal "DC3" = '\DC3'
		escVal "DC4" = '\DC4'

		escVal "NAK" = '\NAK'
		escVal "SYN" = '\SYN'
		escVal "ETB" = '\ETB'
		escVal "CAN" = '\CAN'
		escVal "EM" = '\EM'
		escVal "SUB" = '\SUB'
		escVal "ESC" = '\ESC'
		escVal "FS" = '\FS'
		escVal "GS" = '\GS'
		escVal "RS" = '\RS'
		escVal "US" = '\US'
		escVal "SP" = '\SP'
		escVal "DEL" = '\DEL'
		escVal r@(d:s) | isDigit d = chr n
					     where [(n,_)] = readDec r
		escVal ('o':s) = chr n
				 where [(n,_)] = readOct s
		escVal ('x':s) = chr n
				 where [(n,_)] = readHex s


showLitChar 	:: Char -> ShowS
showLitChar '\\'		= showString "\\\\"
showLitChar c | isPrint c	= showChar c
showLitChar '\a'		= showString "\\a"
showLitChar '\b'		= showString "\\b"
showLitChar '\f'		= showString "\\f"
showLitChar '\n'		= showString "\\n"
showLitChar '\r'		= showString "\\r"
showLitChar '\t'		= showString "\\t"
showLitChar '\v'		= showString "\\v"
showLitChar c	= showChar '\\' . showInt (ord c) . cont
		  where cont s@(c:cs) | isDigit c = "\\&" ++ s
			cont s			  = s


readDec, readOct, readHex :: (Integral a) => ReadS a
readDec = readInt 10 isDigit (\d -> ord d - ord '0')
readOct = readInt  8 (\c -> c >= 0 && c <= 7) (\d -> ord d - ord '0')
readHex = readInt 16 (\c -> isDigit c || c >= 'A' && c <= 'F')
		   (\d -> if isDigit d then ord d - ord '0'
		   		       else ord d - ord 'A' + 10)

readInt :: (Integral a) => a -> (Char -> Bool) -> (Char -> a) -> ReadS a
readInt radix isDig digToInt s =
    [(foldl (\n d -> n * radix + digToInt d) digToInt d, r)
	| (d:ds,r) <- [span isDig s] ]

showInt	:: (Integral a) => a -> ShowS
showInt n = if n < 0 then showChar '-' . showInt' (-n) else showInt' n
           where showInt' n r = chr (ord '0' + d) :
	   			   if n' > 0 then showInt' n' r else r
			       where (n',d) = divRem n 10

-- Standard integral types

instance  Text Int  where
    readsPrec = readIntegral
    showsPrec = showIntegral

instance  Text Integer  where
    readsPrec = readIntegral
    showsPrec = showIntegral

readIntegral p = readParen False read'
		where read' r  = [(-n,t) | ("-",s) <- [lex r],
					   (n,t)   <- [read'' s] ]
		      read'' r = [(n,s)  | (ds,s)  <- [lex r],
		      			   (n,"")  <- readDec ds]

showIntegral p n = showParen (n < 0 && p > 6) (showInt n)


-- Standard floating-point types

instance  Text Float  where
    readsPrec = readFloating
    showsPrec = showFloating

instance  Text Double  where
    readsPrec = readFloating
    showsPrec = showFloating

readFloating p = readParen False read'
	  where read'   r = [(-x,t) | ("-",s) <- [lex r],
				      (x,t)   <- [read'' s] ]
		read''  r = [(fromRational x,t)
				    | (s,t) <- [lex r],
				      (x,"") <- readFix s ++ readSci s]
		readFix r = [(x%1 + y%10^(length t), u)
				    | (x,'.':s) <- readDec r,
				      (t,u)     <- [span isDigit s],
				      y         <- [read t]	   ]
		readSci r = [(x*(10^n%1),t)
				    | (x,'e':s) <- readFix r,
				      (n,t)     <- readDec s ]     ++
			    [(x*(1%10^n),t)
				    | (x,'e':'-':s) <- readFix r,
				      (n,t)	    <- readDec s ]

showFloating p x =
    if p >= 0 then show' x else showParen (p>6) (showChar '-'.show'(-x))
	where
	show' x   = if e >= m || e < 0 then showSci else showFix e
	showSci   = showFix 1 . showChar 'e' . showInt e
	showFix k = showString (fill (take k ds)) . showChar '.'
				. showString (fill (drop k ds))
	fill ds   = if null ds then "0" else ds
	ds        = if sig == 0 then take m (repeat '0') else show sig
	(m, sig, e) = if b == 10 then
		(w, s, if s == 0 then 0 else n+w)
	      else
		(ceiling ((fromInt w * log (fromInteger b))/log 10) + 1,
		 round ((s%1) * (b%1)^^n * 10^^(m-e)),
		 if s == 0 then 0 else floor (logBase 10 x))
	(s, n) = decodeFloat x
	b      = floatRadix x
	w      = floatDigits x


-- Lists

instance  (Text a) => Text [a]  where
    readsPrec p = readList
    showsPrec p = showList


-- Tuples

instance  (Text a, Text b) => Text (a,b)  where
    readsPrec p = readParen False
    	    	    	    (\r -> [((x,y), w) | ("(",s) <- [lex r],
						 (x,t)   <- reads s,
						 (",",u) <- [lex t],
						 (y,v)   <- reads u
						 (")",w) <- [lex v] ] )

    showsPrec p (x,y) = showChar '(' . shows x . showChar ',' .
    	    	    	    	       shows y . showChar ')'
-- et cetera

