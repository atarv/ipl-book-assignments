import           System.Environment             ( getArgs )
import           System.Exit                    ( exitFailure )

import           AbsFun
import           Control.Monad
import           Data.Functor
import           ErrM
import           Interpreter
import           LexFun
import           ParFun
import           PrintFun

import           System.Console.GetOpt
import           System.IO                      ( hGetContents
                                                , stdin
                                                )

newtype Flag = Strategy EvaluationStrategy deriving Show

options :: [OptDescr Flag]
options =
    [ Option "n" ["call-by-name"]  (NoArg $ Strategy CallByName)  ""
    , Option "a" ["call-by-value"] (NoArg $ Strategy CallByValue) ""
    ]

parse :: String -> IO Program
parse s = case pProgram (myLexer s) of
    Left err -> do
        putStrLn "SYNTAX ERROR"
        putStrLn err
        exitFailure
    Right prog -> pure prog

showUsage :: IO ()
showUsage = putStrLn $ usageInfo "ifun" options

main :: IO ()
main = do
    args <- getArgs
    case getOpt RequireOrder options args of
        (_, _, errs@(_ : _)) -> do
            mapM_ putStrLn errs
            exitFailure
        (opts, files, _) -> do
            let evalStrat = if null opts
                    then CallByValue
                    else case head opts of
                        Strategy start -> start
            content <- case files of
                []     -> getContents
                [file] -> readFile file
                xs     -> do
                    putStrLn "Only one file may be interpreted at a time"
                    showUsage
                    exitFailure
            parse content >>= print . interpret evalStrat
