import TreeMap ( Tree(Leaf, Node) )

mapId = fmap id $ Node (Leaf "a") (Leaf "b")
-- This is identical to (Node (Leaf "a") (Leaf "b"))

composed = (fmap even . fmap length) (Just "twelve")
composedIdentical = fmap (even . length) (Just "twelve")