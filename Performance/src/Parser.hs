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

parseTRIPPLE :: Parsec String () (Int, Int, Int)
parseTRIPPLE = do
             char '('
             n1 <- number
             char ','
             n2 <- number
             char ','
             n3 <- number
             char ')'
             return (n1, n2, n3)

parseRANGE :: ParseVal
parseRANGE = do string "RANGE"
                (n1, n2, n3) <- parseTRIPPLE
                return (show <$> takeWhile (<n2) (genvals n1 n3))

parseEXPO :: ParseVal
parseEXPO = do string "EXPO"
               (n1, n2, n3) <- parseTRIPPLE
               return (show <$> takeWhile (<n2) (genvalsexpo n1 n3))

parseVals :: ParseVal
parseVals = parseRANGE <|> parseEXPO

doParse :: String -> [String]
doParse val = fromRight [] $ parse parseVals "" val


replaceValues :: [(String, String)] -> String -> String
replaceValues = flip (List.foldl' replaceValue)

replaceValue :: String -> (String, String) -> String
replaceValue src (from, to) = unpack $ (flip (uncurry Text.replace)) (pack src) (pack from, pack to)
