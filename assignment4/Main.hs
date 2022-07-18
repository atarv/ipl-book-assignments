import           System.Environment             ( getArgs )
import           System.Exit                    ( exitFailure )

import           AbsCPP
import           ErrM
import           LexCPP
import           ParCPP

import           TypeChecker

-- driver

check :: String -> IO ()
check s = case pProgram (myLexer s) of
    Left err -> do
        putStrLn "SYNTAX ERROR"
        putStrLn err
        exitFailure
    Right tree -> case typecheck tree of
        Left err -> do
            putStrLn "TYPE ERROR"
            putStrLn err
            exitFailure
        Right _ -> putStrLn "OK"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> readFile file >>= check
        _      -> do
            putStrLn "Usage: cppi <SourceFile>"
            exitFailure
