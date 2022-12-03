{-# LANGUAGE InstanceSigs #-}
module TreeMap where

data Tree a
    = Node (Tree a) (Tree a)
    | Leaf a

treeLengths :: Foldable t => Tree (t a) -> Tree Int
treeLengths (Leaf s) = Leaf (length s)
treeLengths (Node l r) = Node (treeLengths l) (treeLengths r)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf a) = Leaf (f a)
treeMap f (Node l r) = Node (treeMap f l) (treeMap f r)

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap = treeMap