{-# LANGUAGE OverloadedStrings #-}
module Main.Expectations
    ( shouldHaveColor
    ) where

import qualified Data.Text as T
import qualified Test.WebDriver as WD

import Test.Hspec.WebDriver

-- | Assert the element has the specified CSS color style attribute.
shouldHaveColor :: WD.Element -> T.Text -> WD ()
shouldHaveColor e c = e `WD.cssProp` "color" >>= shouldBe (Just c)
