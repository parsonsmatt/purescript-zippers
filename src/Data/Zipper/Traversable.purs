-- | This module implements the generic zipper on any Traversable data
-- | structure. Code from http://okmij.org/ftp/Haskell/ZipperTraversable.hs
module Data.Zipper.Traversable where

import Prelude

import Control.Monad.Cont.Trans (ContT(ContT), runContT)
import Data.Identity            (Identity(Identity), runIdentity)
import Data.Maybe               (Maybe(..), fromMaybe)
import Data.Traversable         (class Traversable, traverse)

data Zipper t a
    = Done (t a)
    | Zip a (Maybe a -> Zipper t a)

makeZipper :: forall t a. Traversable t => t a -> Zipper t a
makeZipper = reset <<< map Done <<< traverse f
  where
    f a = shift (\k -> return (Zip a (k <<< fromMaybe a)))

zipUp :: forall t a. Zipper t a -> t a
zipUp (Done t) = t
zipUp (Zip _ k) = zipUp (k Nothing)

type Cont r a = ContT r Identity a

cont :: forall a r. ((a -> r) -> r) -> Cont r a
cont f = ContT (\c -> Identity (f (runIdentity <<< c)))

runCont :: forall r a. ContT r Identity a -> (a -> r) -> r
runCont cc k = runIdentity (runContT cc (Identity <<< k))

reset :: forall r. Cont r r -> r
reset m = runCont m id

shift :: forall a r. ((a -> r) -> Cont r r) -> Cont r a
shift e = cont (reset <<< e)
