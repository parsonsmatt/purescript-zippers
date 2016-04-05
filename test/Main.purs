module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Test.QuickCheck.Laws.Data.Functor (checkFunctor)

import Data.Zipper.Tree (TreeZipper(..))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "You should add some tests."
