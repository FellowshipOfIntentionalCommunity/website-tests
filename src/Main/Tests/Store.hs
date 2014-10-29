{-# LANGUAGE OverloadedStrings #-}
module Main.Tests.Store
    ( storeTests
    ) where

import qualified Data.Text as T
import Control.Monad            (unless)
import Test.Hspec.WebDriver

import Main.Colors
import Main.Expectations

storeTests :: Spec
storeTests              = do
        describe "for any page" generalStoreTests
        describe "for general products" productDetailsPageTests
        describe "for recurring products" productWithVariationsDetailsPageTests


-- | Test Store Details that are Visible on Every Page.
generalStoreTests :: Spec
generalStoreTests       = do
        it "opens any page on the website" . runWD $
            openPage "http://www.ic.org/"
        describe "the top rated widget's price" $ do
            it "is prepended by black `From:` text if recurring" . runWD $ do
                e   <- findElems $ ByCSS "p.price span.from"
                unless (null e) $ do
                    head e `shouldHaveText` "From:"
                    head e `shouldHaveColor` black
            priceAmountIsGreen "li"


-- | Test the General Product Details Page.
productDetailsPageTests :: Spec
productDetailsPageTests = do
        it "opens the Product Details page of any product" . runWD $
            openPage "http://www.ic.org/community-bookstore/product/directories-to-libraries/"
        describe "the product price" $ do
            noTextPrependsAmount
            priceAmountIsGreen "p.price"


-- | Test that the amount under an optional parent selector is the correct
-- green.
priceAmountIsGreen :: String -> Spec
priceAmountIsGreen parent = it "is green" . runWD $ do
        e   <- findElem . ByCSS . T.pack $ parent ++ " span.amount"
        e `shouldHaveColor` amountGreen

-- | Test the the Product Details Page of a Product with Multiple
-- Variations.
productWithVariationsDetailsPageTests :: Spec
productWithVariationsDetailsPageTests = do
        it "opens the Product Details page of a recurring product" . runWD $
            openPage "http://www.ic.org/community-bookstore/product/subscription/"
        describe "the product price" $ do
            theCorrectTextPrependsAmount
            priceAmountIsGreen "p.price"

theCorrectTextPrependsAmount :: Spec
theCorrectTextPrependsAmount = it "is prepended by black `From:` text" . runWD $ do
        e   <- findElem $ ByCSS "p.price span.from"
        e `shouldHaveText` "From:"
        e `shouldHaveColor` black

noTextPrependsAmount :: Spec
noTextPrependsAmount = it "is prepended by no text" . runWD $ do
        e   <- findElems $ ByCSS "p.price span.from"
        e `shouldBe` []
