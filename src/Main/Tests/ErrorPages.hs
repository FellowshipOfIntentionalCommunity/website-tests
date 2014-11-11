{-# LANGUAGE OverloadedStrings #-}
{-|

This module tests the HTTP Error Pages(404, 502, etc.).

-}
module Main.Tests.ErrorPages (errorPageTests) where

import Test.Hspec.WebDriver

import Main.Expectations

-- | Test error pages associated with HTTP status codes.
errorPageTests :: Spec
errorPageTests = describe "for 404 pages" error404PageTests

-- | Test the 404 page and any variations.
error404PageTests :: Spec
error404PageTests = do
        it "opens a non-existant page" . runWD $
            openPage "http://www.ic.org/i-am-so-long-that-i-probs-dont-exist"
        it "has the correct title" . runWD $ pageTitleShouldBe
            "Page Not Found - Fellowship for Intentional Community"
        it "has the correct page heading" . runWD $
            pageHeaderShouldBe "Whoops! Page Not Found"
        it "has a search box" . runWD $
            shouldExist $ ByCSS "form#searchform"
        it "opens a non-existant page in the /directory/ sub-URI" . runWD $
            openPage "http://www.ic.org/directory/neither-do-i-hehehe/"
        it "has extra help text about draft listings" . runWD $ do
            e   <- findElem $ ByCSS ".entry-content"
            e `shouldContainText`
                "If you are looking for a Community in the Directory which has "
