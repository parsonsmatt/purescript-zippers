module Data.Zipper.Tree where

import Prelude

import Data.Array (index, updateAt)
import Data.Maybe (Maybe(Nothing))
import Data.Tree (Tree(..))
import Data.List (List(Nil, Cons), (:), uncons)
import Data.Tuple (Tuple(Tuple))

data TreeZipper a
    = TreeZipper (Tree a) (Past a)

type Past a 
    = List (Tuple Int (Tree a))

getTree :: forall a. TreeZipper a -> Tree a
getTree (TreeZipper s _) = s

getPast :: forall a. TreeZipper a -> Past a
getPast (TreeZipper _ p) = p

instance functorTreeZipper :: Functor TreeZipper where
    map f (TreeZipper tree past) =
        TreeZipper (map f tree) (map (map (map f)) past)

up :: forall a. TreeZipper a -> Maybe (TreeZipper a)
up (TreeZipper t past) = do
    { head: Tuple i oldTree@(Tree a ts), tail } <- uncons past
    tree <- Tree a <$> updateAt i oldTree ts
    pure (TreeZipper tree tail)

down :: forall a. Int -> TreeZipper a -> Maybe (TreeZipper a)
down i (TreeZipper t@(Tree a subtrees) past) =
    TreeZipper <$> index subtrees i <*> pure (Tuple i t : past)

leftBy :: forall a. Int -> TreeZipper a -> Maybe (TreeZipper a)
leftBy _ (TreeZipper _ Nil) =
    Nothing
leftBy n tz@(TreeZipper t (Cons (Tuple i past) rest)) =
    up tz >>= down (i + n)

left :: forall a. TreeZipper a -> Maybe (TreeZipper a)
left = leftBy 1

rightBy :: forall a. Int -> TreeZipper a -> Maybe (TreeZipper a)
rightBy n = leftBy (negate n)

right :: forall a. TreeZipper a -> Maybe (TreeZipper a)
right = rightBy 1

sibling :: forall a. Int -> TreeZipper a -> Maybe (TreeZipper a)
sibling n tz = up tz >>= down n

editTree :: forall a. (Tree a -> Tree a) -> TreeZipper a -> TreeZipper a
editTree f (TreeZipper t p) = TreeZipper (f t) p

editFocus :: forall a. (a -> a) -> TreeZipper a -> TreeZipper a
editFocus f = editTree (\(Tree a ts) -> Tree (f a) ts)

singleton :: forall a. a -> TreeZipper a
singleton a = TreeZipper (pure a) Nil
