{-|

This module contains general ic.org tests.

-}
module Main.Tests.General (generalTests) where

import Test.Hspec.WebDriver


-- | Test General ic.org Features
generalTests :: Spec
generalTests    = describe "for the old store domain" oldStoreTests

oldStoreTests :: Spec
oldStoreTests   = do
    it "opens store.ic.org" . runWD $
      openPage "http://store.ic.org"
    it "redirects to the Community Bookstore" . runWD $
      getCurrentURL `shouldReturn` "http://www.ic.org/community-bookstore/"
