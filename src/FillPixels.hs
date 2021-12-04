module FillPixels where

import Text.Read

import FillConf

newtype Col = Col {
    c :: (Double, Double, Double)
} deriving (Read, Eq)

instance Show Col where
    show (Col (r, g, b)) = "(" ++ show (round r) ++ "," ++ show (round g)
        ++ "," ++ show (round b) ++ ")"

data Pixel = Pixel {
    pos :: (Int, Int),
    color :: Col
} deriving (Read)

instance Eq Pixel where
    (Pixel _ (Col c)) == (Pixel _ (Col c')) = c == c'
    (Pixel _ (Col c)) /= (Pixel _ (Col c')) = c /= c'

instance Show Pixel where
    show (Pixel p c) = show p ++ " " ++ show c

lineToPixel :: [String] -> Maybe Pixel
lineToPixel [pos, color] = f (readMaybe pos) (readMaybe color)
    where
        f Nothing _ = Nothing
        f _ Nothing = Nothing
        f (Just p) (Just c) = Just $ Pixel {pos = p, color = Col c}
lineToPixel _ = Nothing

getTabStr :: [String] -> [Maybe Pixel]
getTabStr = (lineToPixel . words <$>)

getPixels :: [String] -> Maybe [Pixel]
getPixels file = sequence $ getTabStr file
