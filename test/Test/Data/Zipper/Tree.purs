module Test.Data.Zipper.Tree (testTreeZipper) where

import Prelude
import Control.Bind
import Control.Plus

import Data.Zipper.Tree
import Data.Tree

import Test.Unit (test, runTest)
import Test.Unit.Assert as Assert
import Control.Monad.Eff.Console (log)

import Test.QuickCheck (Result, (===))
import Test.Unit.QuickCheck

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
    runTest do
        let t = makeZipper (Tree 1 [Tree 2 [], Tree 3 []])
        test "up" do
            Assert.equal (down 0 t >>= up) (pure t)
            Assert.equal (down 1 t >>= up) (pure t)
            Assert.equal (down 2 t >>= up) empty
        test "down" do
            let d1 = getTree <$> down 0 t
                d2 = getTree <$> down 1 t
            Assert.equal d1 (pure (pure 2))
            Assert.equal d2 (pure (pure 3))
        test "left, right" do
            Assert.equal (down 0 t >>= right) (down 1 t)
            Assert.equal (down 1 t >>= left) (down 0 t)
        test "repeated up/down is idempotent" do
            quickCheck upDownProperty
        test "repeated left/right is idempotent" do
            quickCheck leftRightProperty

upDownProperty :: TreeZipper Number -> Result
upDownProperty x =
    down 0 x >>= up >>= down 0 === down 0 x

leftRightProperty :: TreeZipper Number -> Result
leftRightProperty x =
    left x >>= right >>= left >>= right === left x >>= right
