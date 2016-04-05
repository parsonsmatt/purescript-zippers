module Data.Zipper.Tree where

import Prelude

import Data.Array (index, updateAt)
import Data.Maybe (Maybe(Nothing))
import Data.Tree (Tree, View(View), into, out)
import Data.List (List(Nil, Cons), (:))
import Data.Tuple (Tuple(Tuple))

type Past a 
    = List (Tuple Int (Tree a))

data TreeZipper a
    = TreeZipper (Tree a) (Past a)

getTree :: forall a. TreeZipper a -> Tree a
getTree (TreeZipper s _) = s

getPast :: forall a. TreeZipper a -> Past a
getPast (TreeZipper _ p) = p

extractTree :: forall a. Tree a -> a
extractTree t =
    case out t of
         View a _ -> a

instance functorTreeZipper :: Functor TreeZipper where
    map f (TreeZipper tree past) =
        TreeZipper (map f tree) (map (map (map f)) past)

up :: forall a. TreeZipper a -> Maybe (TreeZipper a)
up (TreeZipper _ Nil) =
    Nothing
up (TreeZipper tree (Cons (Tuple i oldTree) past)) =
    case out oldTree of
         View a ts ->
            flip TreeZipper past <<< into <<< View a <$> updateAt i tree ts

down :: forall a. Int -> TreeZipper a -> Maybe (TreeZipper a)
down i (TreeZipper t past) =
    case out t of
         View a subtrees ->
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
editFocus f = editTree g
  where
    g t = case out t of
               View a st ->
                   into (View (f a) st)

singleton :: forall a. a -> TreeZipper a
singleton a = TreeZipper (into (View a [])) Nil
