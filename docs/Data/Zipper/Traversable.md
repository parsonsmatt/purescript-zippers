## Module Data.Zipper.Traversable

This module implements the generic zipper on any Traversable data
structure. Code from http://okmij.org/ftp/Haskell/ZipperTraversable.hs

#### `Zipper`

``` purescript
data Zipper t a
  = Done (t a)
  | Zip a (Maybe a -> Zipper t a)
```

#### `makeZipper`

``` purescript
makeZipper :: forall t a. (Traversable t) => t a -> Zipper t a
```

#### `zipUp`

``` purescript
zipUp :: forall t a. Zipper t a -> t a
```

#### `Cont`

``` purescript
type Cont r a = ContT r Identity a
```

#### `cont`

``` purescript
cont :: forall a r. ((a -> r) -> r) -> Cont r a
```

#### `runCont`

``` purescript
runCont :: forall r a. ContT r Identity a -> (a -> r) -> r
```

#### `reset`

``` purescript
reset :: forall r. Cont r r -> r
```

#### `shift`

``` purescript
shift :: forall a r. ((a -> r) -> Cont r r) -> Cont r a
```


