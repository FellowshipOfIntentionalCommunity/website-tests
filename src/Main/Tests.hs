{-|

This module contains all tests related to the FIC website. The tests
should be contained in sub-modules, with groupings exported through this
module.

-}
module Main.Tests
    ( generalTests
    , errorPageTests
    , eventTests
    , homePageTests
    , storeTests
    ) where

import Main.Tests.ErrorPages
import Main.Tests.Events
import Main.Tests.General
import Main.Tests.HomePage
import Main.Tests.Store
