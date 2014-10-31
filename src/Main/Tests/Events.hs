{-# LANGUAGE OverloadedStrings #-}
{-|

This module tests the Event Pages.

-}
module Main.Tests.Events (eventTests) where

import Test.Hspec.WebDriver

import Main.Expectations

-- | Test the FIC's Events.
eventTests :: Spec
eventTests = describe "for an events details" eventDetailsPageTests


-- | Test the Event Details Page.
eventDetailsPageTests :: Spec
eventDetailsPageTests = do
    it "opens an events details" . runWD $
        openPage "http://www.ic.org/events/spirit-jam-drum-circle-2014-09-12/"
    describe "the events details" $ do
        it "is not empty" . runWD $ do
            e   <- findElem $ ByCSS ".entry-content"
            s   <- getText e
            notElem s [ "", " " ] `shouldBe` True
        it "has a google map with a balloon" . runWD $
            shouldExist $ ByCSS ".em-map-balloon-content"
