module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import LexCPP
import ParCPP
import SkelCPP
import PrintCPP
import AbsCPP

import TypeChecker
import ErrM

runFile :: FilePath -> IO ()
runFile f = readFile f >>= compile

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

compile:: String -> IO ()
compile s = case pProgram (myLexer s) of
  Bad err -> do
    putStrLn "SYNTAX ERROR\n-------------------------\n"
    putStrLn err
    exitFailure
  Ok tree -> case typeCheck tree of
    Bad err -> do
      putStrLn "TYPE ERROR\n-------------------------\n"
      putStrLn err
      exitFailure
    Ok _ -> putStrLn "OK"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> hGetContents stdin >>= compile
    "-s":fs -> mapM_ runFile fs
    fs -> mapM_ runFile fs
