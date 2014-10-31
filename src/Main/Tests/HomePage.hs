{-# LANGUAGE OverloadedStrings #-}
{-|

This module tests the Home Page.

-}
module Main.Tests.HomePage (homePageTests) where

import Test.Hspec.WebDriver

import Main.Expectations

-- | Test the FIC's Home Page.
homePageTests :: Spec
homePageTests = describe "for the home page" $ do
    it "opens the home page" . runWD $
        openPage "http://www.ic.org/"
    describe "the home page" $ do
        it "has the correct title" . runWD $ pageTitleShouldBe
            "Welcome to FIC | The Fellowship for Intentional Community website"
        it "has the correct page heading" . runWD $ pageHeaderShouldBe
            "Welcome to the Fellowship for Intentional Community"
        it "has a link to the login page" . runWD $ do
            e   <- findElem $ ByCSS "#greeting-logout a"
            e `shouldHaveAttr` ("href", "http://www.ic.org/wp-login.php")
            getText e `shouldReturn` "Login"
