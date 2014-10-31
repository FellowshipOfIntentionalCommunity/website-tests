{-# LANGUAGE OverloadedStrings #-}
{-|

This module tests the Web Store.

-}
module Main.Tests.Store (storeTests) where

import qualified Data.Text as T
--import qualified Test.WebDriver as W

import Control.Monad            (unless)
import Test.Hspec.WebDriver

import Main.Colors
import Main.Expectations

-- | Test the FIC's Community Bookstore.
storeTests :: Spec
storeTests              = do
        describe "for any page" generalStoreTests
        describe "for general products" productDetailsPageTests
        describe "for recurring products" recurringProductDetailsPageTests


-- | Test Store Details that are Visible on Every Page.
generalStoreTests :: Spec
generalStoreTests       = do
        it "opens any page on the website" . runWD $
            openPage "http://www.ic.org/"
        describe "the top rated widget's price" $ do
            priceIsCorrectSize
            it "is prefixed by black `From:` text if recurring" . runWD $ do
                e   <- findElems $ ByCSS "ul.product_list_widget li span.from"
                unless (null e) $ do
                    head e `shouldHaveText` "From:"
                    head e `shouldHaveColor` black
            priceAmountIsGreen "li"
            it "is suffixed by black ` / year` text if recurring" . runWD $ do
                es  <- findElems $ ByCSS "ul.product_list_widget li"
                mapM_ (\e -> do recurring   <- findElemsFrom e $ ByCSS "span.from"
                                unless (null recurring) $ do
                                    e `shouldContainText` " / year"
                                    e `shouldHaveColor` black
                      ) es


-- | Test the General Product Details Page.
productDetailsPageTests :: Spec
productDetailsPageTests = do
        it "opens the Product Details page of any product" . runWD $ openPage
            "http://www.ic.org/community-bookstore/product/directories-to-libraries/"
        describe "the product price" $ do
            priceIsCorrectSize
            priceAmountIsGreen "p.price"
            it "is not prefixed or suffixed by any text" . runWD $ do
                amount  <- findElem (ByCSS "p.price span.amount") >>= getText
                price   <- findElem (ByCSS "p.price") >>= getText
                price `shouldBe` amount
        it "opens the Product Details page of a product with a suggested amount" .
            runWD $ openPage
            "http://www.ic.org/community-bookstore/product/website-donation/"
        describe "the name-your-price input" $ do
            it "has a placeholder" $
                pendingWith "See Bug #358"
                -- e   <- findElem (ByCSS "input#nyp")
                -- e `elementShouldHaveAttr` "placeholder"
            it "has no value" $
                pendingWith "See Bug #358"
                -- e   <- findElem (ByCSS "input#nyp")
                -- e `elementShouldNotHaveAttr` "value"


-- | Test the the Product Details Page of a Product with Multiple
-- Variations.
recurringProductDetailsPageTests :: Spec
recurringProductDetailsPageTests = do
        it "opens the Product Details page of a recurring product" . runWD $
            openPage "http://www.ic.org/community-bookstore/product/subscription/"
        describe "the product price" $ do
            priceIsCorrectSize
            theCorrectTextPrefixesAmount
            priceAmountIsGreen "p.price"
            theCorrectTextSuffixesAmount


-- | Test that the amount and any surrounding text is the same, correct size.
priceIsCorrectSize :: Spec
priceIsCorrectSize  = it "is the correct size" $
        pendingWith "See Bug #359"
        --price       <- findElem (ByCSS ".price")
        --amount      <- findElem (ByCSS ".amount")
        --priceSize   <- price `W.cssProp` "font-size"
        --priceSize `shouldBe` Just "14px"
        --amount `W.cssProp` "font-size" `shouldReturn` priceSize

-- | Test that the amount under an optional parent selector is the correct
-- green.
priceAmountIsGreen :: String -> Spec
priceAmountIsGreen parent = it "is green" . runWD $ do
        e   <- findElem . ByCSS . T.pack $ parent ++ " span.amount"
        e `shouldHaveColor` amountGreen

-- | Test that the `from` span has the correct text and color.
theCorrectTextPrefixesAmount :: Spec
theCorrectTextPrefixesAmount = it "is prefixed by black `From:` text"
        $ pendingWith "See Bug #360"
-- . runWD $ do
--        e   <- findElem $ ByCSS "p.price span.from"
--        e `shouldHaveText` "From:"
--        e `shouldHaveColor` black

theCorrectTextSuffixesAmount :: Spec
theCorrectTextSuffixesAmount = it "is suffixed by black ` / year` text"
        $ pendingWith "See Bug #360"
-- . runWD $ do
--        e   <- findElem $ ByCSS "p.price"
--        e `shouldContainText` " / year"
--        e `shouldHaveColor` black
