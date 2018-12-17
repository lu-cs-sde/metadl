module Parser where

import qualified Data.Text as Text
import Data.Text (pack, unpack)

import Text.ParserCombinators.Parsec
import Text.Parsec
import Text.ParserCombinators.Parsec.Char

import qualified Data.List as List
import Data.Either

import Debug.Trace

type ParseVal = Parsec String () [String]



number :: Parsec String () Int
number = many1 digit >>= \n -> return $ read n

genvals :: Int -> Int -> [Int]
genvals start inc = let next = start + inc in next : genvals next inc

genvalsexpo :: Int -> Int -> [Int]
genvalsexpo start fac = let next = start * fac in next : genvalsexpo next fac

parseTuple :: Parsec String () [Int] 
parseTuple = do
    char '('
    n <- number
    ns <- many (char ',' *> number)
    char ')'
    return (n:ns)

parseStringTuple :: Parsec String () [String] 
parseStringTuple = do
    char '('
    n <- many alphaNum
    ns <- many (char ',' *> many alphaNum)
    char ')'
    return (n:ns)

parseTupleLength :: Int -> Parsec String () [a] -> Parsec String () [a] 
parseTupleLength len parser = do
    ns <- parser
    if len /= length ns then fail ("Expected tuple of length: " ++ show len ++ ", but got: " ++ show (length ns)) else return ns

parseTRIPPLE :: Parsec String () (Int, Int, Int)
parseTRIPPLE = do
  ns <- parseTupleLength 3 parseTuple
  return (ns !! 0, ns !! 1, ns !! 2)

parseRANGE :: ParseVal
parseRANGE = do string "RANGE"
                (n1, n2, n3) <- parseTRIPPLE
                return (show <$> takeWhile (<=n2) (genvals n1 n3))

parseEXPO :: ParseVal
parseEXPO = do string "EXPO"
               (n1, n2, n3) <- parseTRIPPLE
               return (show <$> takeWhile (<=n2) (genvalsexpo n1 n3))

readInt :: String -> Int
readInt = read

parseFILERANGE :: ParseVal
parseFILERANGE = do string "FILERANGE"
                    ns <- parseTupleLength 5 parseStringTuple
                    let vals  = show <$> takeWhile (<=(readInt (ns !! 1))) (genvals (readInt (ns !! 0)) (readInt (ns !! 2)))
                        vals' = (\i -> (ns !! 3) ++ i ++ "." ++ (ns !! 4)) <$> vals
                    return vals'

parseFILEEXPO :: ParseVal
parseFILEEXPO = do string "GGG"
                   ns <- parseTupleLength 5 parseStringTuple
                   let vals  = show <$> takeWhile (<=(readInt (ns !! 1))) (genvalsexpo (readInt (ns !! 0)) (readInt (ns !! 2)))
                       vals' = (\i -> (ns !! 3) ++ i ++ "." ++ (ns !! 4)) <$> vals
                   return vals'

parseVals :: ParseVal
parseVals = parseRANGE <|> 
            parseEXPO <|>
            parseFILERANGE <|>
            parseFILEEXPO

doParse :: String -> [String]
doParse val = case parse parseVals "" val of
  Left s -> trace("FAIL: " ++ show s) []
  Right v -> v


replaceValues :: [(String, String)] -> String -> String
replaceValues = flip (List.foldl' replaceValue)

replaceValue :: String -> (String, String) -> String
replaceValue src (from, to) = unpack $ (flip (uncurry Text.replace)) (pack src) (pack from, pack to)
