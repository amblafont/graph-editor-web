module Format.GraphInfo exposing (GraphInfo, Tab, defaultGridSize)
-- the data that we want to copy/save
import Polygraph as Graph
import GraphDefs exposing (EdgeLabel, NodeLabel)

defaultGridSize : Int
defaultGridSize = 200

type alias Tab = 
  { graph : Graph.Graph NodeLabel EdgeLabel,
    title : String,
    active : Bool,
    sizeGrid : Int }

type alias GraphInfo = { tabs : List Tab,
                         latexPreamble : String}

