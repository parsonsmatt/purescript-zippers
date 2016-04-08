## Module Data.Zipper.Tree

#### `TreeZipper`

``` purescript
data TreeZipper a
  = TreeZipper (Tree a) (Past a)
```

A tree zipper is a structure that is useful for navigating and editing a
tree in a purely functional manner. The zipper keeps a reference to a
current tree and a list of past trees.

##### Instances
``` purescript
(Eq a) => Eq (TreeZipper a)
Functor TreeZipper
(Arbitrary a) => Arbitrary (TreeZipper a)
(Coarbitrary a) => Coarbitrary (TreeZipper a)
```

#### `Past`

``` purescript
type Past a = List (Tuple Int (Tree a))
```

The history of a traversal in a tree structure is a list of integer indexes
and the parent tree that was traversed to get there.

#### `getTree`

``` purescript
getTree :: forall a. TreeZipper a -> Tree a
```

Extracts the tree from a tree zipper.

#### `getPast`

``` purescript
getPast :: forall a. TreeZipper a -> Past a
```

Extracts the past trees from a tree zipper.

#### `up`

``` purescript
up :: forall a. TreeZipper a -> Maybe (TreeZipper a)
```

Moves up in the `TreeZipper`. Returns `Nothing` if the zipper is already at
the top.

#### `down`

``` purescript
down :: forall a. Int -> TreeZipper a -> Maybe (TreeZipper a)
```

Moves down into the selected index. Returns `Nothing` if the current tree
does not have a subtree at that index.

#### `leftBy`

``` purescript
leftBy :: forall a. Int -> TreeZipper a -> Maybe (TreeZipper a)
```

Moves left by the given index.

#### `left`

``` purescript
left :: forall a. TreeZipper a -> Maybe (TreeZipper a)
```

Moves the zipper to focus on the tree to the left of the current tree.

#### `rightBy`

``` purescript
rightBy :: forall a. Int -> TreeZipper a -> Maybe (TreeZipper a)
```

Moves the zipper to focus on the tree to the right of the current tree by
the given index.

#### `right`

``` purescript
right :: forall a. TreeZipper a -> Maybe (TreeZipper a)
```

Moves the zipper to focus on the tree to the right of the current tree.

#### `sibling`

``` purescript
sibling :: forall a. Int -> TreeZipper a -> Maybe (TreeZipper a)
```

Moves the zipper to focus on the sibling of the current tree at the given
index.

#### `editTree`

``` purescript
editTree :: forall a. (Tree a -> Tree a) -> TreeZipper a -> TreeZipper a
```

Map a function over the tree at the current index.

#### `editFocus`

``` purescript
editFocus :: forall a. (a -> a) -> TreeZipper a -> TreeZipper a
```

Map a function over the element contained in the current focus.

#### `singleton`

``` purescript
singleton :: forall a. a -> TreeZipper a
```

Create a TreeZipper containing a single element.

#### `tryOr`

``` purescript
tryOr :: forall a. (a -> Maybe a) -> a -> a
```


