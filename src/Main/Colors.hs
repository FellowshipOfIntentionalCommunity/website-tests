module Main.Colors
    ( rgbaColor
    , black
    , amountGreen
    ) where

import qualified Data.Text as T

-- | The string representing Black's CSS style attribute
black :: T.Text
black = rgbaColor 0 0 0

-- | The string represent the green used in the store prices.
amountGreen :: T.Text
amountGreen  = rgbaColor 81 198 32

-- | Transform Integer's describing a  RGB value with an alpha of 1 into
-- a string for use with the 'hasColor' function.
rgbaColor :: Integer -> Integer -> Integer -> T.Text
rgbaColor r g b = T.pack $ "rgba(" ++ show r ++ ", " ++ show g ++ ", " ++ show b
                        ++ ", 1)"
