module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Test.Data.Zipper.Tree (testTreeZipper)

main :: Eff _ Unit
main = do
    testTreeZipper
