-- I/O functions and definitions

module PreludeIO  where

-- File and channel names:

type  Name  = String

stdin	    =  "stdin"
stdout      =  "stdout"
stderr      =  "stderr"
stdecho     =  "stdecho"


-- Requests and responses:

data Request =	-- file system requests:
			  ReadFile      Name         
			| WriteFile     Name String
			| AppendFile    Name String
			| ReadBinFile   Name 
			| WriteBinFile  Name Bin
			| AppendBinFile Name Bin
			| DeleteFile    Name
			| StatusFile    Name
		-- channel system requests:
			| ReadChan	     Name 
			| AppendChan    Name String
			| ReadBinChan   Name 
			| AppendBinChan Name Bin
			| StatusChan    Name
		-- environment requests:
			| Echo          Bool
			| GetArgs
			| GetEnv        Name
			| SetEnv        Name String

data Response =		  Success
			| Str String 
			| Bn  Bin
			| Failure IOError

data IOError =		  WriteError   String
			| ReadError    String
			| SearchError  String
			| FormatError  String
			| OtherError   String


-- Continuation-based I/O:

type Dialogue    =  [Response] -> [Request]
type SuccCont    =                Dialogue
type StrCont     =  String     -> Dialogue
type BinCont     =  Bin        -> Dialogue
type FailCont    =  IOError    -> Dialogue
 
done	      ::                                           Dialogue
readFile      :: Name ->           FailCont -> StrCont  -> Dialogue
writeFile     :: Name -> String -> FailCont -> SuccCont -> Dialogue
appendFile    :: Name -> String -> FailCont -> SuccCont -> Dialogue
readBinFile   :: Name ->           FailCont -> BinCont  -> Dialogue
writeBinFile  :: Name -> Bin    -> FailCont -> SuccCont -> Dialogue
appendBinFile :: Name -> Bin    -> FailCont -> SuccCont -> Dialogue
deleteFile    :: Name ->           FailCont -> SuccCont -> Dialogue
statusFile    :: Name ->           FailCont -> StrCont  -> Dialogue
readChan      :: Name ->           FailCont -> StrCont  -> Dialogue
appendChan    :: Name -> String -> FailCont -> SuccCont -> Dialogue
readBinChan   :: Name ->           FailCont -> BinCont  -> Dialogue
appendBinChan :: Name -> Bin    -> FailCont -> SuccCont -> Dialogue
echo          :: Bool ->           FailCont -> SuccCont -> Dialogue
getArgs	      ::		   FailCont -> StrCont  -> Dialogue
getEnv	      :: Name ->	   FailCont -> StrCont	-> Dialogue
setEnv	      :: Name -> String -> FailCont -> SuccCont -> Dialogue

done resps    =  []

readFile name fail succ resps =
     (ReadFile name) : strDispatch fail succ resps

writeFile name contents fail succ resps =
    (WriteFile name contents) : succDispatch fail succ resps

appendFile name contents fail succ resps =
    (AppendFile name contents) : succDispatch fail succ resps

readBinFile name fail succ resps =
    (ReadBinFile name) : binDispatch fail succ resps

writeBinFile name contents fail succ resps =
    (WriteBinFile name contents) : succDispatch fail succ resps

appendBinFile name contents fail succ resps =
    (AppendBinFile name contents) : succDispatch fail succ resps

deleteFile name fail succ resps =
    (DeleteFile name) : succDispatch fail succ resps

statusFile name fail succ resps =
    (StatusFile name) : strDispatch fail succ resps

readChan name fail succ resps =
    (ReadChan name) : strDispatch fail succ resps

appendChan name contents fail succ resps =
    (AppendChan name contents) : succDispatch fail succ resps

readBinChan name fail succ resps =
    (ReadBinChan name) : binDispatch fail succ resps

appendBinChan name contents fail succ resps =
    (AppendBinChan name contents) : succDispatch fail succ resps

echo bool fail succ resps =
    (Echo bool) : succDispatch fail succ resps

getArgs fail succ resps =
	GetArgs : strDispatch fail succ resps

getEnv name fail succ resps =
	(GetEnv name) : strDispatch fail succ resps

setEnv name val fail succ resps =
	(SetEnv name val) : succDispatch fail succ resps


strDispatch  fail succ (resp:resps) = case resp of 
					Str val      -> succ val resps
					Failure msg  -> fail msg resps

binDispatch  fail succ (resp:resps) = case resp of 
					Bn val       -> succ val resps
					Failure msg  -> fail msg resps

succDispatch fail succ (resp:resps) = case resp of
					Success     -> succ resps
					Failure msg -> fail msg resps


abort		:: FailCont
abort msg	=  done

exit		:: FailCont
exit err	= appendChan stdout msg abort done
		  where msg = case err of ReadError s   -> s
		  			  WriteError s  -> s
		  			  SearchError s -> s
		      			  FormatError s -> s
		      			  OtherError s  -> s


let		::  a -> (a -> b) -> b
let x k		=   k x

print		:: (Text a) => a -> Dialogue
print x		=  appendChan stdout (show x) abort done
prints          :: (Text a) => a -> String -> Dialogue
prints x s	=  appendChan stdout (shows x s) abort done

interact	:: (String -> String) -> Dialogue
interact f	=  readChan stdin abort
			    (\x -> appendChan stdout (f x) abort done)
