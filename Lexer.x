-----------------------------------------------------------------------------
-- Haskell code to initialize the module
-----------------------------------------------------------------------------
{
module Main (main) where

import System.Environment
import System.Directory
import Data.Char
}

-----------------------------------------------------------------------------
-- Alex macro and wrapper definitions
-----------------------------------------------------------------------------
%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]
$newline = [\n]

@comment = \%.* $newline+
@var = $alpha [$alpha $digit \_ \']*
@num = $digit+
@assign = ":="
@add = \+
@sub = \-
@mul = \*
@div = \/
@lpar = "("
@rpar = ")"
@semi = \;

-----------------------------------------------------------------------------
-- Lexer pattern matching definitions
-----------------------------------------------------------------------------
tokens :-
    @comment    ;
    "/*"    {multi}
    $white+     ;
    if      {\(p,_,_,_) i -> return $ TIF p}
    then    {\(p,_,_,_) i -> return $ TTHEN p}
    while   {\(p,_,_,_) i -> return $ TWHILE p}
    do      {\(p,_,_,_) i -> return $ TDO p}
    input   {\(p,_,_,_) i -> return $ TINPUT p}
    else    {\(p,_,_,_) i -> return $ TELSE p}
    begin   {\(p,_,_,_) i -> return $ TBEGIN p}
    end     {\(p,_,_,_) i -> return $ TEND p}
    write   {\(p,_,_,_) i -> return $ TWRITE p}
    @assign {\(p,_,_,_) i -> return $ TASSIGN p}
    @add    {\(p,_,_,_) i -> return $ TADD p}
    @sub    {\(p,_,_,_) i -> return $ TSUB p}
    @mul    {\(p,_,_,_) i -> return $ TMUL p}
    @div    {\(p,_,_,_) i -> return $ TDIV p}
    @lpar   {\(p,_,_,_) i -> return $ TLPAR p}
    @rpar   {\(p,_,_,_) i -> return $ TRPAR p}
    @semi   {\(p,_,_,_) i -> return $ TSEMICOLON p}
    @num    {\(p,_,_,s) i -> return $ TNUM (read (take i s)) p}
    @var    {\(p,_,_,s) i -> return $ TID (take i s) p}
    "*/"    {\alexIn i -> myErr alexIn "Unbalanced Multiline"}
    .{1}    {unknown}

{
-----------------------------------------------------------------------------
-- Taken from Haskell.Language.TH source code then slightly modified
-----------------------------------------------------------------------------
byteToString :: Byte -> Char
byteToString = chr . fromIntegral


-----------------------------------------------------------------------------
-- Generate an error when an unknown character is found in the input string
-----------------------------------------------------------------------------
unknown :: AlexInput -> Int -> Alex Token
unknown (p, c, bs, s) n = myErr (p, c, bs, s) ("Unknown Character: \"" ++ (take 1 s) ++ "\"")

-----------------------------------------------------------------------------
-- Give the error message meaningful output (position of error from input)
-----------------------------------------------------------------------------
myErr :: AlexInput -> String -> Alex Token
myErr ((AlexPn p1 p2 p3), c, bs, s) mssg = error $ mssg ++ " at line " ++ (show p2) ++ ", column " ++ (show p3)

-----------------------------------------------------------------------------
-- A wrapper function to track how many opening multi-line comments need closing
-----------------------------------------------------------------------------
multi :: AlexInput -> Int -> Alex Token
multi alexIn i = do
        currentIn <- alexGetInput
        alexSetInput $ multi' currentIn 1
        alexMonadScan

-----------------------------------------------------------------------------
-- Helper for multi, so the counter can be initialized to 0 at first call
-- The logic is to case over the input string byte-by-byte, discarding anything
-- found that isn't a closing delimeter. However; single line comments take precedent
-- (thus, ignore the entire line following any '%', including closing delimeters).
-----------------------------------------------------------------------------        
multi' :: (Eq a, Num a) => AlexInput -> a -> (AlexPosn, Char, [Byte], String)
multi' alexIn 0 = alexIn
multi' alexIn n = do
        case (alexGetByte alexIn) of
            Nothing -> error "Unbalanced Multi-Line Comment 1"
            Just (nextChar, rest) -> do
                case (byteToString nextChar) of
                    '*' -> do
                        case (alexGetByte rest) of
                            Nothing -> error "Unbalanced Multi-Line Comment 2"
                            Just (nextChar', rest') -> do
                                case (byteToString nextChar') of
                                    '/' -> multi' rest' (n - 1)
                                    _   -> multi' rest' n
                    '/' -> do
                        case (alexGetByte rest) of
                            Nothing -> error "Unbalanced Multi-Line Comment 3"
                            Just (nextChar', rest') -> do
                                case (byteToString nextChar') of
                                    '*' -> multi' rest' (n + 1)
                                    _   -> multi' rest' n
                    '%' -> do
                        case (endLine rest) of
                            Nothing -> error "Unbalanced Multi-Line Comment 4"
                            Just (rest') -> multi' rest' n

                    _   -> multi' rest n


-----------------------------------------------------------------------------
-- This is a helper function for multi' in the case a single line comment is
-- found. This will recursively analyze the string byte-by-byte, only stopping
-- when a new-line is encountered.
-----------------------------------------------------------------------------
endLine :: AlexInput -> Maybe AlexInput
endLine alexIn = do
        case alexGetByte alexIn of
            Nothing -> Nothing
            Just (nextChar, rest) -> do
                case (byteToString nextChar) of
                    '\n'-> Just (rest)
                    _   -> endLine rest


-----------------------------------------------------------------------------
-- This function reads the next token from the input string, and recursively
-- does so until the EOF.
-----------------------------------------------------------------------------
tokens :: [Token] -> Alex [Token]
tokens ts = do
        next <- alexMonadScan
        case next of
            TEOF -> return ts
            otherwise -> tokens (ts ++ [next])

-----------------------------------------------------------------------------
-- The data type for lexical Tokens.
-----------------------------------------------------------------------------
data Token = TIF AlexPosn
            |TTHEN AlexPosn
            |TWHILE AlexPosn
            |TDO AlexPosn
            |TINPUT AlexPosn
            |TELSE AlexPosn
            |TBEGIN AlexPosn
            |TEND AlexPosn
            |TWRITE AlexPosn
            |TID [Char] AlexPosn
            |TNUM Int AlexPosn
            |TADD AlexPosn
            |TASSIGN AlexPosn
            |TSUB AlexPosn
            |TMUL AlexPosn
            |TDIV AlexPosn
            |TLPAR AlexPosn
            |TRPAR AlexPosn
            |TSEMICOLON AlexPosn
            |TEOF
        deriving (Eq)

-----------------------------------------------------------------------------
-- When showing the user the data, the position of the pattern is irrelevant,
-- defining our own instance of Show allows us to maintain the information
-- as it needs to be passed on to the parser eventually, without creating a 
-- mess for debugging.
-----------------------------------------------------------------------------
instance Show Token where
    show tok = case tok of
        TIF _ -> "IF"
        TTHEN _ -> "THEN" 
        TWHILE _ -> "WHILE"
        TDO _ -> "DO"
        TINPUT _ -> "INPUT"
        TELSE _ -> "ELSE"
        TBEGIN _ -> "BEGIN"
        TEND _ -> "END"
        TWRITE _ -> "WRITE"
        TID var _ -> "ID (" ++ (show var) ++ ")"
        TNUM n _ -> "NUM (" ++ (show n) ++ ")"
        TADD _ -> "ADD"
        TASSIGN _ -> "ASSIGN"
        TSUB _ -> "SUB"
        TMUL _ -> "MUL"
        TDIV _ -> "DIV"
        TLPAR _ -> "LPAR"
        TRPAR _ -> "RPAR"
        TSEMICOLON _ -> "SEMICOLON"
        

-----------------------------------------------------------------------------
-- I honestly have no idea why Alex required us to define this ourself.
-----------------------------------------------------------------------------
alexEOF :: Alex Token
alexEOF = return TEOF

-----------------------------------------------------------------------------
-- The main routine of this module (when not served as a stand-alone application)
-----------------------------------------------------------------------------
lexer :: String -> [Token]
lexer inStr = do
        case (runAlex inStr $ tokens []) of
            Right ts -> ts
            Left mssg -> error "Lex Fail"

-----------------------------------------------------------------------------
-- Easier to read than printing out the list in-line. This prints each token
-- on it's own individual line. (I believe, mapM_ is required due to Alex's 
-- monad wrapper)
-----------------------------------------------------------------------------
pPrint :: [Token] -> IO ()
pPrint tokens = mapM_  (putStrLn.show) tokens

-----------------------------------------------------------------------------
-- Take the file name from the argument!
-- I'm allowing the user to pass a flag "-l" as an argument in the case pretty
-- printing is not desired, this will instead print out the list in a single line.
-----------------------------------------------------------------------------
main :: IO ()
main = do
    args <- getArgs
    case (length args) of
        1 -> do
            isFile <- doesFileExist (args !! 0)
            case isFile of
                True -> do
                    content <- readFile (args !! 0)
                    pPrint $ lexer content
                False -> error "Error! File not found."
        2 -> case (args !! 0) of
                "-l" -> do
                    isFile <- doesFileExist (args !! 1)
                    case isFile of
                        True -> do
                            content <- readFile (args !! 1)
                            print $ lexer content
                        False -> error "Error! File not found."
        otherwise -> error "Error! Invalid use. expected either the argument(s) \"./Lexer <filename>\" for pretty printed output, or \"./Lexer -l <filename>\" for the token list as output. "
} 
