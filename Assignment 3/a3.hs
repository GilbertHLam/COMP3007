type Node = Int
type Edge = (Node, Node)
data Graph = Single Node | Union Graph Graph [Edge] deriving Show

checkIfNodeExists :: (Node, Graph) -> Bool
checkIfNodeExists(number, Single number1) = if number == number1
    then True
    else False
checkIfNodeExists(number, Union graphhead graphtail edges ) = checkIfNodeExists(number, graphhead) || checkIfNodeExists(number, graphtail)

-- checkIfEdgeExists :: (Edge, Graph) -> Bool
-- checkIfEdgeExists((node1, node2), edges) = if number == number1
--     then True
--     else False
-- checkIfEdgeExists(number, Union graphhead graphtail edges ) = False || checkIfNodeExists(number, graphhead) || checkIfNodeExists(number, graphtail)

listAllNodes :: (Graph) -> [Node]
listAllNodes(Single number1) = [number1]
listAllNodes(Union graphhead graphtail edges ) = listAllNodes(graphhead)++ listAllNodes(graphtail)

isSingleton :: (Graph) -> Bool
isSingleton(Single number1) = True
isSingleton(Union graphhead graphtail edges) = False