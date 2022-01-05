module Main where

import Prelude
  ( ($)
  , Either(..)
  , Int, (>)
  , String, (++), unlines
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn
  , FilePath
  , getContents, readFile, take, Num ((-)), length
  )
import System.Environment ( getArgs )
import System.Exit        ( exitFailure, exitSuccess )
import System.IO ( stdin, stderr, hGetContents, hPutStrLn, stdout )
import Control.Monad      ( when )
import Data.Maybe
import qualified Data.Map.Lazy as M
import Control.Monad.State

import AbsLatte   ()
import LexLatte   ( Token, mkPosToken )
import ParLatte   ( pProgram, myLexer )
import PrintLatte ( Print, printTree )
import SkelLatte  ()
import StaticAnalysis
import CompileToLLVM

main :: IO ()
main = do
  args <- getArgs
  (fileName, programStr)  <- case args of
                          (fileName:_)  -> do
                            res <- readFile fileName
                            return (fileName, res)
                          _ -> do
                            putStrLn "Specify file to compile"
                            exitFailure
  case pProgram (myLexer programStr) of
    Left err -> do
      putStrLn err
      exitFailure
    Right tree -> case statAnalyze tree of
                    Nothing -> do
                      hPutStrLn stderr "OK\n"
                      --putStrLn $ compile tree
                      compile tree
                    Just s -> hPutStrLn stderr $ "ERROR:\n" ++ s
                    