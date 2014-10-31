{-|

This module defines specific colors used in the FIC's website.

-}
module Main.Colors
    ( rgbaColor
    , black
    , amountGreen
    ) where

import qualified Data.Text as T

-- | Pure black.
black :: T.Text
black = rgbaColor 0 0 0

-- | The green used in the store prices(the `amount` CSS class).
amountGreen :: T.Text
amountGreen  = rgbaColor 81 198 32

-- | Transform Integers describing a RGB value with an alpha of 1 into
-- a string for use with the 'hasColor' expectation.
rgbaColor :: Integer -> Integer -> Integer -> T.Text
rgbaColor r g b = T.pack $ "rgba(" ++ show r ++ ", " ++ show g ++ ", " ++ show b
                        ++ ", 1)"
