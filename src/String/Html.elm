module String.Html exposing (Attribute, Html, text, node, nodeNS, 
  attribute, attributeNS, ghostAttribute, toHtml,
  toString, customNode)

import Html
import Html.Attributes
import Html.String
import Html.String.Attributes
import VirtualDom
{-

A version of html that can be serialised,
inspired by Html.String
-}

type Attribute a = 
     AttributeNS String String String
     -- it will not be translated to string
   | GhostAttribute (Html.Attribute a)
type Html a = NodeNS String String (List (Attribute a)) (List (Html a))
      | TextNode String
      | Custom String (Html.Html a)

attribute : String -> String -> Attribute a
attribute = AttributeNS ""

ghostAttribute : Html.Attribute a -> Attribute a
ghostAttribute = GhostAttribute

attributeNS : String -> String -> String -> Attribute a
attributeNS = AttributeNS

{-
First argument: how it should be translated to string
Second argument: how it should be translated to html
-}
customNode : String -> Html.Html a -> Html a
customNode = Custom

node : String -> List (Attribute a) -> List (Html a) -> Html a
node = NodeNS ""

text : String -> Html a
text = TextNode

nodeNS : String -> String -> List (Attribute a) -> List (Html a) -> Html a
nodeNS = NodeNS

toHtmlStringAttribute : Attribute a -> Maybe (Html.String.Attribute a)
toHtmlStringAttribute attr =
   case attr of
       AttributeNS _ name value -> Html.String.Attributes.attribute name value
            |> Just
       GhostAttribute _ -> Nothing

toHtmlAttribute : Attribute a -> Html.Attribute a
toHtmlAttribute attr =
   case attr of
       AttributeNS nameSpace key value -> 
         (if nameSpace == "" then Html.Attributes.attribute
         else VirtualDom.attributeNS nameSpace)
          key value
       GhostAttribute a -> a


toHtmlString : Html a -> Html.String.Html a
toHtmlString root =
   case root of
      NodeNS _ name attrs children ->
         Html.String.node name 
           (List.filterMap toHtmlStringAttribute attrs)
           (List.map toHtmlString children)
      TextNode s -> Html.String.text s
      Custom s _ -> Html.String.text s

toString : Html a -> String 
toString = toHtmlString >> Html.String.toString 0
   

toHtml : Html a -> Html.Html a 
toHtml root = case root of
      NodeNS nameSpace tagName attrs children ->
         (if nameSpace == "" then VirtualDom.node else VirtualDom.nodeNS nameSpace)
           tagName 
           (List.map toHtmlAttribute attrs)
           (List.map toHtml children)
      TextNode s -> Html.text s
      Custom _ h -> h
   