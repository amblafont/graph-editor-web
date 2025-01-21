port module CommandCodec exposing (protocolReceive, updateModif, updateModifHelper,
          protocolSend, protocolSendModif, protocolSendGraphModif, updateModifHelperWithId
          , scenarioOfString, protocolSendMsg, protocolRequestSnapshot
          -- , protocolAnswerSnapshot, protocolReceiveSnapshot
          )

import Codec exposing (Codec)
import Msg exposing (Command(..), idModifCodec, ModifId, ProtocolMsg(..), ProtocolModif, MoveMode(..), Scenario(..))
import Format.GraphInfoCodec  exposing (codecModif, defaultModifJS)
import Format.GraphInfo as GraphInfo exposing (TabId, GraphInfo)
import Polygraph as Graph
import GraphDefs exposing (NodeLabel, EdgeLabel)
import IntDict
import Model exposing (Model)
import Format.GraphInfo exposing (activeGraphModifHelper)
import IntDictExtra
import Format.LastVersion
import Msg exposing (defaultModifId)
import IntDict
import Geometry.Point exposing (Point)


port protocolReceiveJS : (List {isSender : Bool, msg : ProtocolMsgJS} -> a) -> Sub a
-- if it is a break change (that is, previous diff will be ignored)
-- snapshot: is it a snapshot/checkpoint
-- history: should it be saved in the history
port protocolSendJS : {msg:ProtocolMsgJS, break:Bool, snapshot : Bool
                      ,history:Bool } -> Cmd a

port protocolRequestSnapshot : (() -> a) -> Sub a



scenarioCodec : Codec Scenario String
scenarioCodec = Codec.customEnum
    (\ exo1 simple watch coqlsp standard v ->
       case v of 
         Standard -> standard
         Exercise1 -> exo1 
         SimpleScenario -> simple
         Watch -> watch
         CoqLsp -> coqlsp
    ) -- "" identity (\x _ -> x)
    |> Codec.variant0 "exercise1" Exercise1
    |> Codec.variant0 "simple" SimpleScenario
    |> Codec.variant0 "watch" Watch
    |> Codec.variant0 "coqlsp" CoqLsp
    |> Codec.variant0 "standard" Standard 
    |> Codec.buildVariant (always Standard)

scenarioOfString : String -> Scenario
scenarioOfString = Codec.decoder scenarioCodec
  
updateModif : Model -> GraphInfo.Modif -> (Model, Cmd a)
updateModif model modif = (model, protocolSendModif defaultModifId modif)

updateModifHelperWithId : Model -> ModifId -> Graph.ModifHelper NodeLabel EdgeLabel -> (Model, Cmd a)
updateModifHelperWithId model id modif =
    (model, 
    protocolSendGraphModif model.graphInfo id modif)
    

updateModifHelper : Model -> Graph.ModifHelper NodeLabel EdgeLabel -> (Model, Cmd a)
updateModifHelper model modif = 
   updateModifHelperWithId model defaultModifId modif

protocolReceive : (List {isSender : Bool, msg : ProtocolMsg} -> a) -> Sub a
protocolReceive f = 
    protocolReceiveJS <| f <<
       List.map (\{isSender, msg} -> {isSender = isSender
                     , msg = Codec.decoder protocolMsgCodec 
                    --  <| Debug.log "protocol receive" msg
                    msg
                     })

protocolSendMsg : ProtocolMsg -> Cmd a
protocolSendMsg c =
    let msg = Codec.encoder protocolMsgCodec <| Debug.log "sending msg" c in
    protocolSendJS 
    <| case c of
        Snapshot _ -> {msg = msg, break = False, history = False, 
                     snapshot = True}
        LoadProtocol _ -> {msg = msg, break = True, history = True,
                     snapshot = True}
        ClearProtocol _ -> {msg = msg, break = True, history = True,
                     snapshot = True}
        ModifProtocol _ -> {msg = msg, break = False, history = True, 
                     snapshot = False}
        Undo _ ->  {msg = msg, break = False, history = True, 
                     snapshot = False}
        FocusPosition _ ->
                    {msg = msg, break = False, history = False, 
                     snapshot = False}
      -- :: 
      -- if breakable then [[]
protocolSend : Msg.ProtocolModif -> Cmd a
protocolSend c =
--  if Graph.isEmptyModif c.modif then Cmd.none else
 protocolSendMsg <| Msg.ModifProtocol c

protocolSendModif : ModifId -> GraphInfo.Modif -> Cmd a
protocolSendModif id modif =   
     {id = id, modif = modif, selIds = IntDict.empty,
                              command = Msg.Noop} 
                               |> protocolSend 

protocolSendGraphModif : GraphInfo -> ModifId -> Graph.ModifHelper NodeLabel EdgeLabel -> Cmd a
protocolSendGraphModif g id m =
   let modif = Graph.finaliseModif m in 
   if Graph.isEmptyModif modif then Cmd.none else
   let giModif = GraphInfo.activeGraphModif g modif in
   
      protocolSendModif id giModif
 
type alias ProtocolModifJS = 
    {id : Int, modif : Format.GraphInfoCodec.ModifJS, 
     selIds : List (TabId, List Graph.Id),
     command : CommandJS  }



defaultProtocolMsg : ProtocolMsg 
-- {id : ModifId, modif : Modif, 
--                         selIds : IntDict (List Graph.Id),
--                         command : Command  }
defaultProtocolMsg = 
  ModifProtocol {id = defaultModifId, modif = GraphInfo.Noop, 
  selIds = IntDict.empty, command = Noop}

protocolMsgCodec : Codec ProtocolMsg ProtocolMsgJS
protocolMsgCodec =
    let loadCodec =  
          Codec.object (\ scenario graph -> {scenario = scenario, graph = graph})
                       (\ scenario graph -> {scenario = scenario, graph = graph})
                    |> Codec.fields .scenario .scenario scenarioCodec 
                    |> Codec.fields .graph .graph Format.LastVersion.graphInfoCodec
                    |> Codec.buildObject
    in
    let clearCodec =  
          Codec.object (\ scenario preamble -> {scenario = scenario, preamble = preamble})
                       (\ scenario preamble -> {scenario = scenario, preamble = preamble})
                    |> Codec.fields .scenario .scenario scenarioCodec 
                    |> Codec.fields .preamble .preamble Codec.identity
                    |> Codec.buildObject
    in
    let splitMsg snapshot clear undo load modif focus v = 
           case v of 
               Snapshot arg -> snapshot arg
               ModifProtocol arg -> modif arg
               LoadProtocol arg -> load arg
               ClearProtocol arg -> clear arg
               Undo arg -> undo arg
               FocusPosition arg -> focus arg
               
    in
    Codec.maybeCustom splitMsg 
    (
   \ snapshot clear undo load modif focus -> {snapshot = snapshot,
      clear = clear, undo = undo, load = load, modif = modif, focus = focus }
    )
    |> Codec.maybeVariant1 Snapshot .snapshot Format.LastVersion.graphInfoCodec
    |> Codec.maybeVariant1 ClearProtocol .clear clearCodec
    |> Codec.maybeVariant1 Undo .undo (Codec.list codecModif)
    |> Codec.maybeVariant1 LoadProtocol .load loadCodec
    |> Codec.maybeVariant1 ModifProtocol .modif protocolModifCodec
    |> Codec.maybeVariant1 FocusPosition .focus Codec.identity
    |> Codec.maybeBuildVariant defaultProtocolMsg


   
type alias ProtocolMsgJS = {
  modif : Maybe ProtocolModifJS,
  undo : Maybe (List Format.GraphInfoCodec.ModifJS),
  load : Maybe { graph : Format.LastVersion.Graph, scenario : String },
  clear : Maybe { preamble : String, scenario : String },
  snapshot : Maybe Format.LastVersion.Graph,
  focus : Maybe {tabId : TabId, pos : Point, selIds : List Graph.Id}
  }

      
  


protocolModifCodec : Codec ProtocolModif ProtocolModifJS
protocolModifCodec =
   Codec.object 
    (\id modif selIds command -> {id = id, modif = modif, selIds = selIds, command = command} )
    (\id modif selIds command -> {id = id, modif = modif, selIds = selIds, command = command} )
    |> Codec.fields .id .id idModifCodec 
    |> Codec.fields .modif .modif codecModif 
    |> Codec.fields .selIds .selIds IntDictExtra.codec 
    |> Codec.fields .command .command commandCodec
    |> Codec.buildObject

-- Generated by the type-checker
type alias CommandJS = 
  { --  tag : String, 
    rename : Maybe (List {id : Graph.Id, tabId : TabId, label : Maybe String})
    , moveMode : Maybe String
    , noop : Bool
  }
    
    -- createPoint : { isMath : Bool, tabId : TabId, pos : Point },
    -- rename : { modifs : List Format.GraphInfoCodec.ModifJS
    --          , next : List { id : Graph.Id, label : String, isLabel : Bool, tabId : TabId }
    --           }
              --  }

moveModeCodec : Codec MoveMode String
moveModeCodec =
    Codec.customEnum (\ press undefined free v ->
       case v of 
          PressMove -> press
          FreeMove -> free
          UndefinedMove -> undefined
    ) --"" identity (\x _ -> x)
    |> Codec.variant0 "press" PressMove
    |> Codec.variant0 "undefined" UndefinedMove
    |> Codec.variant0 "free" FreeMove
    |> Codec.buildVariant (always FreeMove)

defaultCommand : Command
defaultCommand = Noop


commandCodec : Codec Command CommandJS
commandCodec =
   let split move rename noop v =
          case v of
                -- LoadCommand arg -> load arg
                MoveCommand arg -> move arg
                RenameCommand arg -> rename arg
                -- CreatePoint arg -> createPoint arg
                -- Rename r -> rename r
                Noop -> noop
   in
   Codec.maybeCustom split 
     (\ move rename noop -> {moveMode = move, rename = rename, noop = noop})
   |> Codec.maybeVariant1 MoveCommand .moveMode moveModeCodec
   |> Codec.maybeVariant1 RenameCommand .rename Codec.identity
   |> Codec.maybeVariant0 Noop .noop
   |> Codec.maybeBuildVariant defaultCommand
