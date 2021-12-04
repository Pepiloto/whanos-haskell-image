module Parsing where

import Data.Maybe

import FillConf

getUsage :: String
getUsage = "USAGE: ./imageCompressor -n N -l L -f F\n\n" ++
    "\tN\tnumber of colors in the final image\n" ++
    "\tL\tconvergence limit\n" ++
    "\tF\tpath to the file containing the colors of the pixels"

checkErrors :: Conf -> Int -> Maybe Conf
checkErrors conf@(Conf a b c) i
    | a == 0 || b == 0 || i /= 3 = Nothing
    | otherwise = Just conf

fillArgs :: [String] -> Conf -> Int -> Maybe Conf
fillArgs [] _ 0 = Nothing
fillArgs [] conf i
    | isNothing $ checkErrors conf i = Nothing
    | otherwise = Just conf
fillArgs ("-n":tl1:tl2) conf i = fillArgs tl2 (getNbColors tl1 conf) (i + 1)
fillArgs ("-l":tl1:tl2) conf i = fillArgs tl2 (getLimConv tl1 conf) (i + 1)
fillArgs ("-f":tl1:tl2) conf i = fillArgs tl2 conf (i + 1)
fillArgs _ _ _ = Nothing
