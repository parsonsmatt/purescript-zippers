module Test.Data.Zipper.Tree (testTreeZipper) where

import Prelude

import Data.Zipper.Tree

import Control.Monad.Eff.Console (log)

import Type.Proxy (Proxy(..), Proxy2(..))

import Test.QuickCheck.Laws.Control.Comonad (checkComonad)
import Test.QuickCheck.Laws.Control.Apply (checkApply)
import Test.QuickCheck.Laws.Control.Applicative (checkApplicative)
import Test.QuickCheck.Laws.Control.Bind (checkBind)
import Test.QuickCheck.Laws.Control.Monad (checkMonad)
import Test.QuickCheck.Laws.Control.Extend (checkExtend)
import Test.QuickCheck.Laws.Data.Functor (checkFunctor)
import Test.QuickCheck.Laws.Data.Eq (checkEq)

proxy2 :: Proxy2 TreeZipper
proxy2 = Proxy2

proxyN :: Proxy (TreeZipper Number)
proxyN = Proxy

testTreeZipper = do
  log "Testing Laws for Instances"
  checkEq proxyN
  checkFunctor proxy2
--  checkApply proxy2
--  checkApplicative proxy2
--  checkBind proxy2
--  checkMonad proxy2
--  checkExtend proxy2
--  checkComonad proxy2
