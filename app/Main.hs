module Main where

import System.Environment
import System.Exit ( ExitCode(ExitFailure), exitWith )

import ClusterFinding
import Parsing
import Algo
import FillConf
import FillPixels

getNbCol :: Int -> Int -> Int
getNbCol nbCol len
    | nbCol >= len = len
    | otherwise = nbCol

algo :: Conf -> [Pixel] -> IO ()
algo _ [] = putStrLn "File incorrect" >> exitWith (ExitFailure 84)
algo c@(Conf nbCol conv _) p = fillClustersRd (getNbCol nbCol (length p)) p >>=
    (\clList -> printCluster nbCol $ setClusterAndPixel clList p conv)

getPixelList :: Conf -> IO [Pixel]
getPixelList c@(Conf _ _ buff) = case getPixels buff of
    Nothing -> putStrLn "File incorrect" >> exitWith (ExitFailure 84)
    Just pixelTab -> return pixelTab

checkArgs :: [String] -> IO Conf
checkArgs args = case fillArgs args (Conf 0 0 []) 0 of
        Nothing -> putStrLn getUsage >> exitWith (ExitFailure 84)
        Just c -> return c

strSplit :: [String] -> Conf -> IO Conf
strSplit args (Conf nb lim _) =
    readFile (last args) >>= return . Conf nb lim . lines

main :: IO ()
main = getArgs >>= \args -> checkArgs args >>= strSplit args >>=
    \conf -> getPixelList conf >>= algo conf
