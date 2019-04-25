module Main where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import LexGram
import ParGram
import SkelGram
import PrintGram
import AbsGram

import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

main :: IO ()
main = do
    return ()
