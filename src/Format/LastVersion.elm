module Format.LastVersion exposing (Graph, toJSGraph, fromJSGraph, version)

import Format.Version11 as LastFormat
type alias Graph = LastFormat.Graph
toJSGraph = LastFormat.toJSGraph
fromJSGraph = LastFormat.fromJSGraph
version = LastFormat.version
