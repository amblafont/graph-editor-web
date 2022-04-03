module Format.GraphInfo exposing (GraphInfo, defaultGridSize, makeGraphInfo)
-- the data that we want to copy/save
import Polygraph as Graph
import GraphDefs exposing (EdgeLabel, NodeLabel)

defaultGridSize : Int
defaultGridSize = 200

type alias GraphInfo = { graph : Graph.Graph NodeLabel EdgeLabel, sizeGrid : Int}

makeGraphInfo : Graph.Graph NodeLabel EdgeLabel -> Int -> GraphInfo
makeGraphInfo = GraphInfo

