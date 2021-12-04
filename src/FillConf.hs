module FillConf where

import Data.Maybe
import Text.Read

data Conf = Conf {
    nbColors :: Int,
    limConv :: Double,
    pathColors :: [String]
} deriving (Eq, Show, Read)

getNbColors :: String -> Conf -> Conf
getNbColors str (Conf _ b c) =
    Conf (fromMaybe 0 (readMaybe str :: Maybe Int)) b c

getLimConv :: String -> Conf -> Conf
getLimConv str (Conf a _ c) =
    Conf a (fromMaybe 0 (readMaybe str :: Maybe Double)) c
