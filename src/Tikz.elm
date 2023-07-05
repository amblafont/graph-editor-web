module Tikz exposing (graphToTikz)

import GraphDefs exposing (NodeLabel, EdgeLabel, EdgeType(..), GenericEdge)
import Polygraph as Graph exposing (Graph, Node, Edge)
import Maybe.Extra
import Geometry exposing (LabelAlignment(..))
import ArrowStyle

encodeNodeTikZ : Int -> Node NodeLabel -> String
encodeNodeTikZ sizeGrid n =
    -- TODO: faire la normalisation
    let (x, y) = n.label.pos in
    let coord u = (u / 21) in -- 17.7667
    "\\node ("
        ++ String.fromInt n.id
        ++ ") at ("
        ++ String.fromFloat (coord x)
        ++ "em, "
        ++ String.fromFloat (0 - coord y)
        ++ "em) {$"
        ++ (if n.label.label == "" then "\\bullet" else n.label.label )
        ++ "$} ; \n"

encodeFakeEdgeTikZ : Edge EdgeLabel -> String
encodeFakeEdgeTikZ e =
    "("
        ++ String.fromInt e.from
        ++ ") to["
        ++ encodeFakeLabel e
        ++ "] node[coordinate]("
        ++ String.fromInt e.id
        ++ "){} ("
        ++ String.fromInt e.to
        ++ ") \n"


encodeFakeLabel : Edge EdgeLabel -> String
encodeFakeLabel e =
    case e.label.details of
        PullshoutEdge -> ""
        NormalEdge l -> ArrowStyle.tikzStyle l.style

graphToTikz : Int -> Graph NodeLabel EdgeLabel -> String
graphToTikz sizeGrid g =
    let gnorm = g |> Graph.normalise in
    let nodes = Graph.nodes gnorm in
    let all_edges = Graph.edges gnorm in
    let (edges, pullshouts) = 
           List.partition 
           (GraphDefs.filterEdgeNormal >> Maybe.Extra.isJust)
             all_edges 
    in
    let tikzNodes = nodes |> List.map (encodeNodeTikZ sizeGrid) |> String.concat
    in
    let tikzFakeEdges =
            "\\path \n" ++ (edges |> List.map encodeFakeEdgeTikZ |> String.concat) ++ "; \n"
    in
    let tikzEdges =
            "\\path[->] \n" ++ (List.sortBy (.label >> .zindex) edges |> List.map encodeEdgeTikZ |> String.concat) ++ "; \n"
    in
    let tikzPullshouts =
            pullshouts |> List.map (encodePullshoutTikZ gnorm) |> String.concat
    in
    "\\begin{tikzpicture}[every node/.style={inner sep=5pt,outer sep=0pt,anchor=base,text height=1.2ex, text depth=0.25ex}] \n"
        ++ tikzNodes
        ++ tikzFakeEdges
        ++ tikzEdges
        ++ tikzPullshouts
        ++ "\\end{tikzpicture}"

encodePullshoutTikZ : Graph NodeLabel EdgeLabel -> Edge EdgeLabel -> String
encodePullshoutTikZ g e =
    case ( Graph.getEdge e.from g, Graph.getEdge e.to g ) of
        ( Just s, Just t ) ->
            let
                ( a, b, c ) =
                    if s.to == t.to then
                        ( String.fromInt s.from, String.fromInt s.to, String.fromInt t.from )

                    else
                        ( String.fromInt s.to, String.fromInt s.from, String.fromInt t.to )
            in
            "\\pullbackk{"
                ++ a
                ++ "}{"
                ++ b
                ++ "}{"
                ++ c
                ++ "}{draw} % \n"

        ( _, _ ) ->
            "raté!"

encodeEdgeTikZ : Edge EdgeLabel -> String
encodeEdgeTikZ e =
    "("
        ++ String.fromInt e.from
        ++ ") edge["
        ++ encodeLabel e
        ++ "] ("
        ++ String.fromInt e.to
        ++ ") \n"

encodeLabel : Edge EdgeLabel -> String
encodeLabel e =
    case e.label.details of
        PullshoutEdge ->
            ""

        NormalEdge l ->
            let lbl = "$\\scriptstyle " ++ 
                    String.replace "," "{,}" l.label 
                    ++ "$"
            in
            (case l.style.labelAlignment of
                 Over -> "labelonat={" ++ lbl ++ "}{" ++ String.fromFloat l.style.labelPosition ++ "}, "
                 Centre -> "labelonat={" ++ lbl ++ "}{" ++ String.fromFloat l.style.labelPosition ++ "}, "                 
                 Left -> "\"" ++ lbl ++ "\", "
                 Right -> "\"" ++ lbl ++ "\"', ")
            ++ "pos=" ++ String.fromFloat l.style.labelPosition ++ ", "
            ++ ArrowStyle.tikzStyle l.style

labelfromAlignment : LabelAlignment -> String
labelfromAlignment a =
    case a of
        Centre -> "labeloat"
        Over -> "labelonat"
        Left -> "labelrat"
        Right -> "labellat"
