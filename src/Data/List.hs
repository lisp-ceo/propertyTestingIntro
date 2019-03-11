module Data.List(BST) where

data BST k v =
  Leaf | Branch (BST k v) k v (BST k v)
  deriving (Eq, Show)

insert k v Leaf =
  Branch Leaf k v Leaf

insert k v (Branch l k' v' r)
  | k<k'  = Branch (insert k v l) k' v' r
  | k>k'  = Branch l k' v' (insert k v r)
  | k==k' = Branch l k' v r
