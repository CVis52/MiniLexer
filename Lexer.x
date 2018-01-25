{
module Main (main) where

import System.Environment
import Data.Char
}

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

tokens :-
    @comment    ;
    "/*"    {multi}
    $white+     ;
    if      {\(p,_,_,_) i -> return $ IF p}
    then    {\(p,_,_,_) i -> return $ THEN p}
    while   {\(p,_,_,_) i -> return $ WHILE p}
    do      {\(p,_,_,_) i -> return $ DO p}
    input   {\(p,_,_,_) i -> return $ INPUT p}
    else    {\(p,_,_,_) i -> return $ ELSE p}
    begin   {\(p,_,_,_) i -> return $ BEGIN p}
    end     {\(p,_,_,_) i -> return $ END p}
    write   {\(p,_,_,_) i -> return $ WRITE p}
    @assign {\(p,_,_,_) i -> return $ ASSIGN p}
    @add    {\(p,_,_,_) i -> return $ ADD p}
    @sub    {\(p,_,_,_) i -> return $ SUB p}
    @mul    {\(p,_,_,_) i -> return $ MUL p}
    @div    {\(p,_,_,_) i -> return $ DIV p}
    @lpar   {\(p,_,_,_) i -> return $ LPAR p}
    @rpar   {\(p,_,_,_) i -> return $ RPAR p}
    @semi   {\(p,_,_,_) i -> return $ SEMICOLON p}
    @num    {\(p,_,_,s) i -> return $ NUM (read (take i s)) p}
    @var    {\(p,_,_,s) i -> return $ ID (take i s) p}
    "*/"    {\alexIn i -> myErr alexIn "Unbalanced Multiline"}
    .{1}    {unknown}

{
-- Taken from Haskell.Language.TH source code then slightly modified
byteToString :: Byte -> Char
byteToString = chr . fromIntegral

unknown :: AlexInput -> Int -> Alex Token
unknown (p, c, bs, s) n = myErr (p, c, bs, s) ("Unknown Character: \"" ++ (take 1 s) ++ "\"")

myErr :: AlexInput -> String -> Alex Token
myErr ((AlexPn p1 p2 p3), c, bs, s) mssg = error $ mssg ++ " at line " ++ (show p2) ++ ", column " ++ (show p3)

multi :: AlexInput -> Int -> Alex Token
multi alexIn i = do
        currentIn <- alexGetInput
        alexSetInput $ multi' currentIn 1
        alexMonadScan
        

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


endLine :: AlexInput -> Maybe AlexInput
endLine alexIn = do
        case alexGetByte alexIn of
            Nothing -> Nothing
            Just (nextChar, rest) -> do
                case (byteToString nextChar) of
                    '\n'-> Just (rest)
                    _   -> endLine rest


tokens :: [Token] -> Alex [Token]
tokens ts = do
        next <- alexMonadScan
        case next of
            TEOF -> return ts
            otherwise -> tokens (ts ++ [next])

data Token = IF AlexPosn
            |THEN AlexPosn
            |WHILE AlexPosn
            |DO AlexPosn
            |INPUT AlexPosn
            |ELSE AlexPosn
            |BEGIN AlexPosn
            |END AlexPosn
            |WRITE AlexPosn
            |ID [Char] AlexPosn
            |NUM Int AlexPosn
            |ADD AlexPosn
            |ASSIGN AlexPosn
            |SUB AlexPosn
            |MUL AlexPosn
            |DIV AlexPosn
            |LPAR AlexPosn
            |RPAR AlexPosn
            |SEMICOLON AlexPosn
            |TEOF
        deriving (Eq, Show)

alexEOF :: Alex Token
alexEOF = return TEOF

lexer inStr = do
        case (runAlex inStr $ tokens []) of
            Right ts -> ts
            Left ts -> error "Lex Fail"

main = do
    file <- getArgs
    s <- readFile (file !! 0)
    print $ lexer s

} 
