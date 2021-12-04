module ClusterFinding where

import System.Random
import Data.List

import FillConf
import FillPixels

data Cluster = Cluster {
    color :: Col,
    pixList :: [Pixel]
} deriving (Show, Read, Eq)

initOneClusterRd :: [Pixel] -> IO Pixel
initOneClusterRd l = randomRIO (0, (length l) - 1) >>=
    (\idx -> return $ l !! idx)

fillClustersRd :: Int -> [Pixel] -> IO [Cluster]
fillClustersRd 0 _ = return []
fillClustersRd nbColor pixels = do
    (Pixel _ c) <- initOneClusterRd pixels
    l <- fillClustersRd (nbColor - 1) pixels
    return ((Cluster c []) : l)