type Node = Int
type Edge = (Node, Node)
data Graph = Single Node | Union Graph Graph [Edge] deriving Show