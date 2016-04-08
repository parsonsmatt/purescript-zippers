module Data.Zipper.Tree where

import Prelude

import Data.Array (index, updateAt, length)
import Data.Maybe (Maybe(), fromMaybe)
import Data.Tree (Tree(..))
import Data.List (List(Nil), (:), uncons)
import Data.Tuple (Tuple(Tuple))

import Test.QuickCheck.Arbitrary (class Coarbitrary, class Arbitrary,
                                 coarbitrary, arbitrary)
import Test.QuickCheck.Gen (sized, elements, chooseInt)

-- | A tree zipper is a structure that is useful for navigating and editing a
-- | tree in a purely functional manner. The zipper keeps a reference to a
-- | current tree and a list of past trees.
data TreeZipper a
    = TreeZipper (Tree a) (Past a)

-- | The history of a traversal in a tree structure is a list of integer indexes
-- | and the parent tree that was traversed to get there.
type Past a
    = List (Tuple Int (Tree a))

-- | Creates a `TreeZipper` from a `Tree`.
makeZipper :: forall a. Tree a -> TreeZipper a
makeZipper = flip TreeZipper Nil

-- | Extracts the tree from a tree zipper.
getTree :: forall a. TreeZipper a -> Tree a
getTree (TreeZipper s _) = s

-- | Extracts the past trees from a tree zipper.
getPast :: forall a. TreeZipper a -> Past a
getPast (TreeZipper _ p) = p

-- | Moves up in the `TreeZipper`. Returns `Nothing` if the zipper is already at
-- | the top.
up :: forall a. TreeZipper a -> Maybe (TreeZipper a)
up (TreeZipper t past) = do
    { head: Tuple i (Tree a ts), tail } <- uncons past
    tree <- Tree a <$> updateAt i t ts
    pure (TreeZipper tree tail)

-- | Moves down into the selected index. Returns `Nothing` if the current tree
-- | does not have a subtree at that index.
down :: forall a. Int -> TreeZipper a -> Maybe (TreeZipper a)
down i (TreeZipper t@(Tree a subtrees) past) =
    TreeZipper <$> index subtrees i <*> pure (Tuple i t : past)

-- | Moves left by the given index.
leftBy :: forall a. Int -> TreeZipper a -> Maybe (TreeZipper a)
leftBy n tz@(TreeZipper t past) = do
    { head: Tuple i past, tail } <- uncons past
    tz' <- up tz
    down (i - n) tz'

-- | Moves the zipper to focus on the tree to the left of the current tree.
left :: forall a. TreeZipper a -> Maybe (TreeZipper a)
left = leftBy 1

-- | Moves the zipper to focus on the tree to the right of the current tree by
-- | the given index.
rightBy :: forall a. Int -> TreeZipper a -> Maybe (TreeZipper a)
rightBy n = leftBy (negate n)

-- | Moves the zipper to focus on the tree to the right of the current tree.
right :: forall a. TreeZipper a -> Maybe (TreeZipper a)
right = rightBy 1

-- | Moves the zipper to focus on the sibling of the current tree at the given
-- | index.
sibling :: forall a. Int -> TreeZipper a -> Maybe (TreeZipper a)
sibling n tz = up tz >>= down n

-- | Map a function over the tree at the current index.
editTree :: forall a. (Tree a -> Tree a) -> TreeZipper a -> TreeZipper a
editTree f (TreeZipper t p) = TreeZipper (f t) p

-- | Map a function over the element contained in the current focus.
editFocus :: forall a. (a -> a) -> TreeZipper a -> TreeZipper a
editFocus f = editTree (\(Tree a ts) -> Tree (f a) ts)

-- | Create a TreeZipper containing a single element.
singleton :: forall a. a -> TreeZipper a
singleton a = TreeZipper (pure a) Nil

instance eqTreeZipper :: Eq a => Eq (TreeZipper a) where
    eq (TreeZipper x xs) (TreeZipper y ys) = eq x y && eq xs ys

instance showTreeZipper :: Show a => Show (TreeZipper a) where
    show (TreeZipper t p) =
        "TreeZipper { getTree = " <> show t <> ", getPast = " <> show p <> " }"


instance functorTreeZipper :: Functor TreeZipper where
    map f (TreeZipper tree past) =
        TreeZipper (map f tree) (map (map (map f)) past)

instance arbitraryTreeZipper :: Arbitrary a => Arbitrary (TreeZipper a) where
    arbitrary = do
        init <- initial
        sized (randomWalk init)
      where
        initial = TreeZipper <$> arbitrary <*> pure Nil
        randomWalk tz 0 = pure tz
        randomWalk tz@(TreeZipper (Tree _ subtrees) _) n = do
            i <- chooseInt 0 (length subtrees - 1)
            f <- elements id (map tryOr [up, down i, left, right])
            randomWalk (f tz) (n - 1)

instance coarbitraryTreeZipper :: Coarbitrary a => Coarbitrary (TreeZipper a) where
    coarbitrary (TreeZipper t p) = coarbitrary t <<< coarbitrary p

tryOr :: forall a. (a -> Maybe a) -> a -> a
tryOr f = fromMaybe <*> f
