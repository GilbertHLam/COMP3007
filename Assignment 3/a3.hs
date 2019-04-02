type Node = Int
type Edge = (Node, Node)
data Graph = Single Node | Union Graph Graph [Edge] deriving Show

-- Question 1
checkIfNodeExists :: (Node, Graph) -> Bool
checkIfNodeExists(number, Single number1) = if number == number1
    then True
    else False
checkIfNodeExists(number, Union graphhead graphtail edges ) = checkIfNodeExists(number, graphhead) || checkIfNodeExists(number, graphtail)

-- Question 2
isEdgeInList :: (Edge, [Edge]) -> Bool
isEdgeInList(edge, []) = False
isEdgeInList(edge, head:tail) = if edge == head 
    then True
    else isEdgeInList(edge, tail)

checkIfEdgeExists :: (Edge, Graph) -> Bool
checkIfEdgeExists(edge, Single number1) = False
checkIfEdgeExists(edge, Union graphhead graphtail edges ) = if isEdgeInList(edge, edges) == True
    then True
    else checkIfEdgeExists(edge, graphhead) || checkIfEdgeExists(edge, graphtail)

-- Question 3
listAllNodes :: (Graph) -> [Node]
listAllNodes(Single number1) = [number1]
listAllNodes(Union graphhead graphtail edges ) = listAllNodes(graphhead) ++ listAllNodes(graphtail)

-- Question 4
listAllEdges :: (Graph) -> [Edge]
listAllEdges(Single number1) = []
listAllEdges(Union graphhead graphtail edges) = edges ++ listAllEdges(graphhead) ++ listAllEdges(graphtail)

-- Question 5
isSingleton :: (Graph) -> Bool
isSingleton(Single number1) = True
isSingleton(Union graphhead graphtail edges) = False

traverseList :: (Graph, Node) -> [Node]
traverseList()