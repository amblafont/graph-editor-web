module Format.GraphInfoCodec exposing (ModifJS, codecModif, codecGraphModif, defaultModifJS)

import Format.GraphInfo exposing (..)
import Format.LastVersion as LastVersion exposing (tabCodec)
import Codec exposing (Codec)
import Polygraph as Graph exposing (Graph)
import GraphDefs exposing (EdgeLabel, NodeLabel)
import Polygraph exposing (modifCodec)
import IntDictExtra


defaultGraphModifJS : Graph.ModifJS LastVersion.NodeLabel LastVersion.EdgeLabel
defaultGraphModifJS = Graph.emptyModifHelper |> Graph.finaliseModif
    |> Codec.encoder codecGraphModif



codecGraphModif : Codec (Graph.Modif NodeLabel EdgeLabel) 
                        (Graph.ModifJS LastVersion.NodeLabel LastVersion.EdgeLabel)
codecGraphModif = 
  Codec.compose 
     Graph.modifCodec
     (Graph.mapModifCodec LastVersion.nodeCodec LastVersion.edgeCodec)

{-
(WithSelRen {tabId : TabId,
                 modif : Graph.Modif NodeLabel EdgeLabel })
-}
{- 
type alias WithSelRen a = { a |
-- tabId --> Grpah.Id 
        selIds : IntDict (List Graph.Id)
      , renIds : List { id : Graph.Id, tabId : TabId, label : String}
      }
-}
-- codecWithSelRen : Codec (WithSelRen a)
--                         {a | selIds = List (TabId, List Graph.Id)
--                         , renIds 
--                         }

{-
codecGraphChange : Codec (WithSelRen {tabId : TabId,
                 modif : Graph.Modif NodeLabel EdgeLabel })  
                 { selIds : List ( TabId, List Graph.Id )
         , modif : Polygraph.ModifJS LastVersion.NodeLabel LastVersion.EdgeLabel
         , renIds : List {id : Graph.Id, tabId : TabId, label : String, isLabel : Bool}
         , tabId : TabId }
         
codecGraphChange = 
   Codec.object 
   (\ tabId renIds modif selIds  -> 
       {tabId = tabId 
       , modif = modif 
       , selIds = selIds
       , renIds = renIds } )
    (\ tabId renIds modif selIds  -> 
       {tabId = tabId 
       , modif = modif 
       , selIds = selIds
       , renIds = renIds } )
   |> Codec.fields .tabId .tabId Codec.identity 
   |> Codec.fields .renIds .renIds 
       (Codec.object (\id tabId label -> {id = id, tabId = tabId, label = label})
            (\id tabId label -> {id = id, tabId = tabId, label = label.value, isLabel = label.isValue})
         |> Codec.fields .id .id Codec.identity 
         |> Codec.fields .tabId .tabId Codec.identity
         |> Codec.fields .label (\{label, isLabel} -> {value = label, isValue = isLabel})
              (Codec.maybe "")
         |> Codec.buildObject
         |> Codec.list
       )
   |> Codec.fields .modif .modif codecGraphModif
   |> Codec.fields .selIds .selIds IntDictExtra.codec
   |> Codec.buildObject
-}


-- generated by the type-checker
type alias ModifJS = 
   { tag : String, size : Int, tab : LastVersion.Tab, string : String, 
     tabId : TabId, 
     graphModif : Polygraph.ModifJS LastVersion.NodeLabel LastVersion.EdgeLabel  
    
   }


defaultModifJS : ModifJS
defaultModifJS = {tag = "", tabId = 0, string = "", size = 0
         , tab = emptyTab 0 |> Codec.encoder tabCodec
         , graphModif = defaultGraphModifJS
            
             }

codecModif : Codec Modif ModifJS
codecModif =
    -- let makeCommon tabId string = {tabId = tabId, string = string} in
    let updTabId id r = { r | tabId = id } in

    Codec.customStringTag
    (\ tabRename tabSizeGrid
        tabMoveLeft tabMoveRight
        tabRemove tabUnremove tabDuplicate 
        tabNew  latexPreamble graphChange noop v ->
        case v of
            TabRename id s -> tabRename id s
            TabSizeGrid id size -> tabSizeGrid id size
            TabMoveLeft id -> tabMoveLeft id
            TabMoveRight id -> tabMoveRight id
            TabRemove id -> tabRemove id
            TabUnremove tab -> tabUnremove tab
            TabDuplicate id -> tabDuplicate id
            TabNew -> tabNew
            LatexPreamble s -> latexPreamble s
            GraphChange arg -> graphChange arg
            Noop -> noop
    )
    defaultModifJS
   --  .tag 
   --  (\ tag m -> {m | tag = tag})

    |> Codec.variant2 "tabRename" TabRename 
       (\ tabId string r -> { r | tabId = tabId, string = string }) .tabId .string 
    |>  Codec.variant2 "tabSizeGrid" TabSizeGrid 
       (\ tabId size r -> { r | tabId = tabId, size = size }) .tabId .size 
    |> Codec.variant1 "tabMoveLeft" TabMoveLeft updTabId .tabId Codec.identity
    |> Codec.variant1 "tabMoveRight" TabMoveRight updTabId .tabId Codec.identity
    |> Codec.variant1 "tabRemove" TabRemove updTabId .tabId Codec.identity
    |> Codec.variant1 "tabUnremove" TabUnremove 
       (\ tab r -> { r | tab = tab }) .tab tabCodec
    |> Codec.variant1 "tabDuplicate" TabDuplicate updTabId .tabId Codec.identity
    |> Codec.variant0 "tabNew" TabNew
    |> Codec.variant1 "latexPreamble" LatexPreamble (\ s r -> { r | string = s }) .string Codec.identity
    |> Codec.variant1 "graphChange" GraphChange 
            (\ s r -> { r | graphModif = s.modif, tabId = s.tabId })
            ( \{graphModif, tabId} -> {modif = graphModif, tabId = tabId})
            (Codec.object
                (\modif tabId -> {modif = modif, tabId = tabId})
                (\modif tabId -> {modif = modif, tabId = tabId})
              |> Codec.fields .modif .modif codecGraphModif
              |> Codec.fields .tabId .tabId Codec.identity
              |> Codec.buildObject
            )
    |> Codec.variant0 "noop" Noop
    |> Codec.buildVariant (always Noop)