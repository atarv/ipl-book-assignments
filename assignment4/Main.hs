import           System.Environment             ( getArgs )
import           System.Exit                    ( exitFailure )

import           AbsCPP
import           ErrM
import           LexCPP
import           ParCPP
import           PrintCPP

import           CodeGen                        ( compileProgram )
import           System.IO                      ( hGetContents
                                                , stdin
                                                )
import           System.Process
import           TypeChecker

-- driver

check :: String -> IO Program
check s = case pProgram (myLexer s) of
    Left err -> do
        putStrLn "SYNTAX ERROR"
        putStrLn err
        exitFailure
    Right program -> case typecheck program of
        Left err -> do
            putStrLn "TYPE ERROR"
            putStrLn err
            exitFailure
        Right prog -> pure prog

compile :: FilePath -> Program -> IO ()
compile file prog = do
    let assembly     = compileProgram prog
        assemblyFile = file <> ".j"
    writeFile assemblyFile assembly
    callProcess "jasmin" [assemblyFile]

main :: IO ()
main = do
    args <- getArgs
    case args of
        []     -> getContents >>= check >>= compile "stdin"
        [file] -> readFile file >>= check >>= compile file
        _      -> do
            putStrLn "Usage: cppi <SourceFile>"
            exitFailure
