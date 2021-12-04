module Algo where

import FillConf
import FillPixels
import ClusterFinding

findElemFloat :: [Double] -> Double -> Int -> Int
findElemFloat (x:xs) searched idx = if x == searched
                                    then idx
                                    else findElemFloat xs searched (idx + 1)

closest :: [Cluster] -> (Double, Double, Double) -> Cluster
closest l c = l !! findElemFloat distances (minimum distances) 0
    where distances = (\(Cluster (Col c') _) -> distance c' c) `fmap` l

distance :: (Double, Double, Double) -> (Double, Double, Double) -> Double
distance (r1, g1, b1) (r2, g2, b2) = sqrt (r' * r' + g' * g' + b' * b')
    where
        r' = r1 - r2
        g' = g1 - g2
        b' = b1 - b2

assignPixToCluster :: Pixel -> [Cluster] -> Cluster -> [Cluster]
assignPixToCluster p c t@(Cluster col pixL) = begin ++ new ++ end
    where
        (begin, _:end) = span (/= t) c
        new = [Cluster col (pixL ++ [p])]

linkClusterAndPixel :: [Cluster] -> [Pixel] -> [Cluster]
linkClusterAndPixel c [] = c
linkClusterAndPixel c (x@(Pixel _ (Col col)):xs) =
    linkClusterAndPixel newCl xs
    where
        nearest = closest c col
        newCl = assignPixToCluster x c nearest

addPixel :: Pixel -> [Pixel] -> Pixel
addPixel p [] = p
addPixel (Pixel pos (Col (r, g, b))) ((Pixel _ (Col (r', g', b'))) : xs) =
    addPixel (Pixel pos (Col (r + r', g + g', b + b'))) xs

getAverage :: [Pixel] -> Pixel
getAverage l = Pixel (0, 0) (Col average)
    where
        (Pixel _ (Col (r, g, b))) = addPixel (Pixel (0, 0) (Col (0, 0, 0))) l
        len = fromIntegral (length l)
        average = (r / len, g / len, b / len)

newCluster :: [Cluster] -> [Cluster]
newCluster [] = []
newCluster (x@(Cluster c []):xs) = Cluster c [] : newCluster xs
newCluster (x@(Cluster _ pixL):xs) = Cluster newColor [] : newCluster xs
    where (Pixel _ newColor) = getAverage pixL

checkDistance :: [Cluster] -> [Cluster] -> Double -> Bool
checkDistance [] [] _ = True
checkDistance (x@(Cluster (Col vec) _):xs) (x'@(Cluster (Col vec') _):xs') c =
    (distance vec vec' < c) && checkDistance xs xs' c

setClusterAndPixel :: [Cluster] -> [Pixel] -> Double -> [Cluster]
setClusterAndPixel c p conv = if checkDistance newCl newCl' conv
                              then newCl'
                              else setClusterAndPixel newCl' p conv
    where
        newCl = linkClusterAndPixel c p
        newCl' = linkClusterAndPixel (newCluster newCl) p

printPixels :: [Pixel] -> IO ()
printPixels = foldr ((>>) . print) (return ())

printCluster :: Int -> [Cluster] -> IO ()
printCluster 0 [] = return ()
printCluster nb [] = putStrLn "--" >> putStrLn "-" >> printCluster (nb - 1) []
printCluster nb ((Cluster c l):xs) = putStrLn "--" >> print c >>
    putStrLn "-" >> printPixels l >> printCluster (nb - 1) xs
