module Main where

import qualified System.Environment as Env
import System.Exit
import System.Process
import System.FilePath
import System.IO
import Control.Monad


import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Char as Char

import Debug.Trace
import Parser
import GenData

type ReplaceSingle = (String, String)
type ReplaceEntry  = (String, [String])
type ReplaceMap    = [ReplaceEntry]
type Template      = String
type Program       = String

replaceDelim = ":"

replaceMap :: [String] -> ReplaceMap
replaceMap = List.foldl' splitter []
    where
      splitter acc s = let spl = Split.splitOn replaceDelim s in (spl !! 0, doParse $ spl !! 1)  : acc

program :: Template -> ReplaceMap -> Program
program = undefined


replacements :: [[ReplaceSingle]] -> [[ReplaceSingle]]
replacements []         = [[]]
replacements (rss:rsss) = rss >>= \rs ->
             replacements rsss >>= \rss' ->
             return $ rs : rss'

replacePartition :: ReplaceMap -> [[ReplaceSingle]]
replacePartition rm = rm >>= \(base, rss) -> [(,) base <$> rss]

programs :: Template -> ReplaceMap -> [Program]
programs t rm = (flip replaceValues t) <$> replacements (replacePartition rm)

runProgram :: FilePath -> Program -> IO [String]
runProgram fp program = do
  writeFile ("../examples/" ++ takeFileName fp) program
  strs <- forM [1..10] (const $ readCreateProcess (shell cmd_run_prog) "" >>= \s -> putStr (s ++ " ") >> return s)
  putStrLn ""
  return strs 
  where
    cmd_run_prog   = "cd ..; java -jar compiler.jar eval::souffle -OUT ./out -FACTS ./facts ./examples/" ++ takeFileName fp
    {- cmd_run_prog   = "cd ..; java -jar compiler.jar eval::bottomupnaive -OUT ./out -FACTS ./facts ./examples/" ++ takeFileName fp -}

collectSample :: Template -> ReplaceMap -> FilePath -> IO [[Double]]
collectSample template replMap infile = do
  {- forM_ (programs template replMap) (putStrLn . show) -}
  samples <- traverse (runProgram infile) (programs template replMap)
  return $ ((read :: (String -> Double)) <$> ) <$> samples
      
main :: IO ()
main = do args <- Env.getArgs
          let infile   = args !! 0
              replfile = args !! 1
          template <- readFile infile
          replace <- readFile replfile
          writePDataExpo 1 1000000 2

          let replMap = replaceMap $ lines replace
          -- putStrLn template 
          putStrLn (show replMap)
          vals <- collectSample template replMap infile
          putStrLn . show $ vals
