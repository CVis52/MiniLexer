module Main (main) where

import qualified Lexer as L
import System.Environment
import System.Directory
import Rainbow

defDir = "profTests/"
errMsg = fore red
sucMsg = fore green

main = do
    args <- getArgs
    putChunkLn $ (errMsg  (chunk $ sep' 77))
    case (length args) of
        1 -> tester (args !! 0) 
        _ -> tester defDir


tester path = do
    isDir <- doesDirectoryExist path
    case isDir of
        True -> do
            dirCont <- getDirectoryContents path
            let testFiles = getMs path dirCont
            runTests testFiles
        False -> error $ path ++ "Directory Not Found"


printPaths :: [[Char]] -> IO ()
printPaths [] = putStrLn ""
printPaths (f:fs) = do
        putStrLn $ f ++ "\n"
        printPaths fs

getMs :: String -> [String] -> [String]
getMs path [] = []
getMs path (f:fs) = case isM f of
    True -> (path ++ f) : (getMs path fs)
    False -> getMs path fs

isM "" = False
isM (s:str) = if (s == '.') then (restM str) else (isM str)
restM str = if (str == "m-") then True else False

runTests :: [FilePath] -> IO ()
runTests paths = runTests' 0 paths

runTests' :: Int -> [FilePath] -> IO ()
runTests' n [] = print ""
runTests' n (f:fs) = do 
        runTest (n + 1) f
        runTests' (n + 1) fs

runTest :: Int -> FilePath -> IO ()
runTest n f = do
        cont <- readFile f
        putStrLn ("File: " ++ f ++ "\n" ++ (sep f 6))
        let result = L.lexer cont
        putStrLn (show $ result)
        putStrLn ("\n" ++ sep' 77 ++ "\n" ++ sep' 31 ++ "  End of Test  " ++ (show n) ++ " " ++ sep' (30 - (length $ show n)) ++ "\n" ++ sep' 77 ++ "\n\n\n")

sep f n = sep' $ n + (length f)
sep' 0 = ""
sep' n = '=' : (sep' $ n-1)
