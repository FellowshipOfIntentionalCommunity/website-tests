{-# LANGUAGE OverloadedStrings #-}
{-|

This module is the test driver, it organizes the test categories into
a single Spec and runs it.

-}
module Main (main) where

import Test.Hspec.WebDriver

import Main.Tests

-- | Assemble and test all Specs for the FIC's ic.org website.
main :: IO ()
main = hspec $ session "ic.org Tests" . using Chrome $ do
        describe "Home Page Tests" homePageTests
        describe "Error Page Tests" errorPageTests
        describe "Events Tests" eventTests
        describe "Store Tests" storeTests
