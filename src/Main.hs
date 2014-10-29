{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Hspec.WebDriver

import Main.Tests

main :: IO ()
main = hspec $
    parallel . session "ic.org Tests" $ using Chrome $
        describe "Store Tests" storeTests
