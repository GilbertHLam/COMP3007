-- Name: Gilbert Lam
-- Student ID: 101044511
-- COMP3007 Assignment 3
-- 2019-03-16
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

-- Question 6
traverseList :: (Graph, Node) -> [Node]
traverseList(Single number1, node) = []
traverseList(Union graphhead graphtail edges, node) = supportFunction(Union graphhead graphtail edges, node,[])

-- Question 7
createGraph :: [(Node, [Node])] -> Graph
createGraph([]) = error "No Adjacency List"
createGraph([(node, edges), (node2, edge2)]) = Union (Single node) (Single node2) (createEdge(node, edges) ++ createEdge(node2, edge2))
createGraph((node, edges):tail) = Union (Single node) (createGraph(tail)) (createEdge(node, edges))

-- Question 8
createGraphFromMatrix :: [[Bool]] -> Graph
createGraphFromMatrix([]) = error "Empty matrix!"
createGraphFromMatrix(list) = if checkIfMatrixIsSquare(list)
    then createGraph(convertMatrixToList(1, list))
    else error "Non-square matrix!"


-- Question 8 Helpers
checkIfMatrixIsSquare :: [[Bool]] -> Bool
checkIfMatrixIsSquare(h:t) = if getLengthArray(h:t) == getLengthBool(h)
    then True
    else False

convertMatrixToList :: (Int,[[Bool]]) -> [(Node, [Node])]
convertMatrixToList(number, []) = []
convertMatrixToList(number, head: tail) = [convertRowToList(head, number)] ++ convertMatrixToList(number+1, tail)

convertRowToList :: ([Bool], Node) -> (Node, [Node])
convertRowToList(list, node) = (node, convertArrayOfBool(getLengthBool list,list))

convertArrayOfBool :: (Int,[Bool]) -> [Node]
convertArrayOfBool(length,[]) = []
convertArrayOfBool(length, h:t) = if h == True
    then [length - getLengthBool(t)] ++ convertArrayOfBool(length, t)
    else [] ++ convertArrayOfBool(length, t)


-- Question 7 Helpers
createEdge :: (Node,[Node]) -> [Edge]
createEdge(node, []) = []
createEdge(node,h:t) = [(node, h)] ++ createEdge(node,t)

-- Question 6 Helpers
findNodeFrom :: (Node, [Edge]) -> [Node]
findNodeFrom(node, []) = []
findNodeFrom(node, (node1, node2):tail) = if node == node1
    then [node2] ++ findNodeFrom(node, tail)
    else findNodeFrom(node, tail)

sortListByNode :: [Node] -> [Node]
sortListByNode([]) = []
sortListByNode (h:t) =   
    let small = sortListByNode [a | a <- t, a <= h]  
        big = sortListByNode [a | a <- t, a > h]  
    in  small ++ [h] ++ big

getFirstElementInList:: [Node] -> Node
getFirstElementInList([h]) = h
getFirstElementInList(h:t) = h

getLength:: [Node] -> Int
getLength([]) = 0
getLength(h:t) = 1 + getLength(t)

getLengthBool:: [Bool] -> Int
getLengthBool([]) = 0
getLengthBool(h:t) = 1 + getLengthBool(t)

getLengthArray:: [[Bool]] -> Int
getLengthArray([]) = 0
getLengthArray(h:t) = 1 + getLengthArray(t)

supportFunction :: (Graph, Node, [Node]) -> [Node]
supportFunction(Single number1, node, list) = list
supportFunction(Union graphhead graphtail edges, node, list) = if getLength(list) == getLength(listAllNodes(Union graphhead graphtail edges)) - 1
    then list
    else if getLength(list) == 0
        then [node] ++ supportFunction(Union graphhead graphtail edges, getFirstElementInList(sortListByNode(findNodeFrom(node, listAllEdges(Union graphhead graphtail edges)))), list ++ sortListByNode(findNodeFrom(node, listAllEdges(Union graphhead graphtail edges)))) 
        else supportFunction(Union graphhead graphtail edges, getFirstElementInList(sortListByNode(findNodeFrom(node, listAllEdges(Union graphhead graphtail edges)))), list ++ sortListByNode(findNodeFrom(node, listAllEdges(Union graphhead graphtail edges)))) 

--Question 2 Helper
isEdgeInList :: (Edge, [Edge]) -> Bool
isEdgeInList(edge, []) = False
isEdgeInList(edge, head:tail) = if edge == head 
    then True
    else isEdgeInList(edge, tail)
