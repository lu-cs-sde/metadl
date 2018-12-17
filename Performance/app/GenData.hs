module GenData where

import Parser
import Control.Monad
import qualified Data.List as List

genPData :: Int -> [(String, String)]
genPData n = let l = [1..n] in (\(p, c) -> ("P" ++ show p, "P" ++ show c)) <$> zip l (drop 1 l)

prettyData :: [(String, String)] -> String
prettyData = List.foldl' (\acc (p,c) -> (p ++ "," ++ c ++ "\n") ++ acc) ""

writePData :: Int -> IO ()
writePData n = do
    putStrLn ("Begin Write" ++ show n)
    {- putStrLn $ show (length (prettyData . genPData $ n)) -}
    writeFile ("../facts/parent" ++ show n ++ ".csv") (prettyData . genPData $ n)
    putStrLn "End Write"

writePDataRange :: Int -> Int -> Int -> IO ()
writePDataRange n1 n2 n3 = forM_ (takeWhile (<=n2) (genvals n1 n3)) writePData

writePDataExpo :: Int -> Int -> Int -> IO ()
writePDataExpo n1 n2 n3 = forM_ (takeWhile (<=n2) (genvalsexpo n1 n3)) writePData
