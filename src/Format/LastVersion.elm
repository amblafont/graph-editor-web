module Format.LastVersion exposing (Graph, NodeLabel, EdgeLabel, defaultGraph, nodeCodec, edgeCodec, toJSGraph, fromJSGraph, version, Tab, tabCodec, graphInfoCodec)

import Format.Version16 as LastFormat
type alias Graph = LastFormat.Graph
type alias Tab = LastFormat.Tab
type alias NodeLabel = LastFormat.Node
type alias EdgeLabel = LastFormat.Edge
defaultGraph = LastFormat.defaultGraph
tabCodec = LastFormat.tabCodec
nodeCodec = LastFormat.nodeCodec
edgeCodec = LastFormat.edgeCodec
toJSGraph = LastFormat.toJSGraph
fromJSGraph = LastFormat.fromJSGraph
version = LastFormat.version
graphInfoCodec = LastFormat.graphInfoCodec
