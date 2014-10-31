{-# LANGUAGE OverloadedStrings #-}
{-|

This module contains functions that describe expectations we have for the
FIC website.

-}
module Main.Expectations
    ( pageTitleShouldBe
    , pageHeaderShouldBe
    , shouldExist
    , shouldContainText
    , shouldHaveColor
    , shouldHaveFontSize
    , elementShouldHaveAttr
    , elementShouldNotHaveAttr
    ) where

import qualified Data.Text as T
import qualified Test.WebDriver as W

import Data.Maybe               (isJust, isNothing)
import Test.Hspec.WebDriver

-- | Assert a Page's Title is equal to a string.
pageTitleShouldBe :: T.Text -> WD ()
pageTitleShouldBe t             = getTitle `shouldReturn` t

-- | Assert a Page's h1 tag is equal to a string.
pageHeaderShouldBe :: T.Text -> WD ()
pageHeaderShouldBe t            = do s  <- findElem (ByCSS "h1")
                                     getText s `shouldReturn` t

-- | Assert that at least one Element exists for the Selector.
shouldExist :: W.Selector -> WD ()
shouldExist s            = do es <- findElems s
                              null es `shouldBe` False

-- | Assert that an Element has specific text somewhere within it.
shouldContainText :: W.Element -> T.Text -> WD ()
shouldContainText e t    = do et <- getText e
                              t `T.isInfixOf` et `shouldBe` True

-- | Assert the element has the specified CSS color style attribute.
shouldHaveColor :: W.Element -> T.Text -> WD ()
shouldHaveColor e c             = e `W.cssProp` "color" `shouldReturn` Just c

-- | Assert the element has the specified font size.
shouldHaveFontSize :: W.Element -> T.Text -> WD ()
shouldHaveFontSize e s          = e `W.cssProp` "font-size" `shouldReturn` Just s

-- | Assert the Element has the specified attribute.
elementShouldHaveAttr :: W.Element -> T.Text -> WD ()
elementShouldHaveAttr e a       = e `attr` a >>= ((`shouldBe` True) . isJust)

-- | Assert the Element does not have the specified attribute.
elementShouldNotHaveAttr :: W.Element -> T.Text -> WD ()
elementShouldNotHaveAttr e a    = e `attr` a >>= ((`shouldBe` True) . isNothing)
