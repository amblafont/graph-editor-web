-- inspired by https://package.elm-lang.org/packages/miniBill/elm-codec/latest/
module Codec exposing
    ( Codec, build
    , identity 
    , decoder 
    , encoder
    -- , enum
    , customEnum
    , customStringTag
    , list
    , ObjectCodec, object, compose, composite
    , fields, buildObject 
    , custom,  variant1, buildVariant
    , variant2,variant0
    , maybeCustom, maybeBuildVariant, maybeVariant0, maybeVariant1
   -- , maybe
    -- , variant0, variant2
    )

{-
two ways of encoding custom types: custom and maybeCustom
-}

import List.Extra as List
import Dict exposing (Dict)



-- DEFINITION


{-| A value that knows how to encode and decode JSON values.
-}
type Codec a b
    = Codec
        { encoder : a -> b
        , decoder : b -> a
        }


identity : Codec a a 
identity = Codec {encoder = Basics.identity, decoder = Basics.identity}


-- DECODE



{-| Extracts the `Decoder` contained inside the `Codec`.
-}
decoder : Codec a b -> b -> a
decoder (Codec m) =
    m.decoder



-- ENCODE


{-| Extracts the encoding function contained inside the `Codec`.
-}
encoder : Codec a b -> a -> b
encoder (Codec m) =
    m.encoder



-- BASE


{-| Build your own custom `Codec`.
Useful if you have pre-existing `Decoder`s you need to use.
-}
build : (a -> b) -> (b -> a) -> Codec a b
build encoder_ decoder_ =
    Codec
        { encoder = encoder_
        , decoder = decoder_
        }



{-
enum : List ( a, b ) -> (a, b) -> Codec a b
enum l (defaultA, defaultB) =
    let enc defaultVal proj1 proj2  val =
           List.find (proj1 >> (==) val) l
           |> Maybe.map proj2
           |> Maybe.withDefault defaultVal
        
    in
    Codec { encoder = enc defaultB Tuple.first Tuple.second ,
            decoder = enc defaultA Tuple.second Tuple.first
          }
-}



-- DATA STRUCTURES


composite : ((b -> d) -> (a -> c)) -> ((d -> b) -> (c -> a))
            -> Codec b d -> Codec a c
composite enc dec (Codec codec) =
    Codec
        { encoder = enc codec.encoder
        , decoder = dec codec.decoder
        }

compose : Codec b c -> Codec a b -> Codec a c
compose dec enc =
    Codec
        { encoder = encoder enc >> encoder dec
        , decoder = decoder dec >> decoder enc
        }


list : Codec a b -> Codec (List a) (List b)
list =
    composite List.map List.map


-- OBJECTS


{- | A partially built `Codec` for an object.
b1 and b2 are function types
a1 and a2 are the final types
-}
type ObjectCodec a1 a2 b1 b2
    = ObjectCodec
        { -- encoder : a1 -> List ( String, Value )
          encoder  : a1 -> b2 -- Decoder b1 a1
        , decoder  : a2 -> b1
        }


{-| Start creating a `Codec` for an object. You should pass the main constructor as argument.
If you don't have one (for example it's a simple type with no name), you should pass a function that given the field values builds an object.

Example with constructor:

    type alias Point =
        { x : Float
        , y : Float
        }

  

     type alias Point2 =
        { x2 : Float
        , y2 : Float
        }

    pointCodec : Codec Point Point2
    pointCodec =
        Codec.object Point Point2
            |> Codec.fields .x .x2 Codec.identity
            |> Codec.fields .y .y2 Codec.identity
            |> Codec.buildObject





-}
object : b1 -> b2 -> ObjectCodec a1 a2 b1 b2
object b1 b2 =
    ObjectCodec
        { 
          encoder = always b2
        , decoder = always b1
        }

fields : (a1 -> f1) -> (a2 -> f2)
    -> Codec f1 f2
    ->  ObjectCodec a1 a2 (f1 -> b1) (f2 -> b2) 
    ->  ObjectCodec a1 a2 b1 b2
fields getter1 getter2 cod (ObjectCodec ocodec) =
    ObjectCodec {
        encoder = \ a1 -> ocodec.encoder a1 <| encoder cod <| getter1 a1
      , decoder = \ a2 -> ocodec.decoder a2 <| decoder cod <| getter2 a2
    }

{-| Create a `Codec` from a fully specified `ObjectCodec`.
-}
buildObject : ObjectCodec a1 a2 a1 a2 -> Codec a1 a2
buildObject (ObjectCodec om) =
    Codec
        { encoder = om.encoder
        , decoder = om.decoder
        }

{-
Codec with {tag, snapshot, clear, load, modif}
  let splitMsg snapshot clear load modif v = 
           case v of 
               Snapshot arg -> snapshot arg
               ModifProtocol arg -> modif arg
               LoadProtocol arg -> load arg
               ClearProtocol arg -> clear arg
    in
    Codec.customStringTag splitMsg defaultProtocolMsgJS
    |> Codec.variant1 "snapshot" Snapshot (\ n r -> {r | snapshot = n}) .snapshot Format.LastVersion.graphInfoCodec
    |> Codec.variant1 "clear" ClearProtocol (\ n r -> {r | clear = n}) .clear clearCodec
    |> Codec.variant1 "load" LoadProtocol (\ n r -> {r | load = n}) .load loadCodec
    |> Codec.variant1 "modif" ModifProtocol (\ m r -> { r | modif = m}) .modif protocolModifCodec
    |> Codec.buildVariant
-}
type CustomCodec v tag r a x = CustomCodec
    { encoder : a
    -- , make : tag -> common -> r
    , getTag : r -> tag
    , setTag : tag -> r -> r
    -- , getCommon : r -> common

    , decoder : Dict tag (r -> v) 
    , defaultDecoder : r -> x
    , defaultCommon : r
    }

-- type alias CustomType tag content = {tag : tag, content:content}

custom : a -> r -> (r -> tag) -> (tag -> r -> r) 
             -> CustomCodec v tag r a r
custom a common getTag setTag = 
   CustomCodec { encoder = a
    , decoder = Dict.empty
    , getTag = getTag
    , setTag = setTag
    , defaultCommon = common
    , defaultDecoder = Basics.identity
    }

customStringTag : a -> { b | tag : String} -> 
                 CustomCodec v String {b | tag : String} a {b | tag : String}
customStringTag a common =
    custom a common .tag (\ tag r -> {r | tag = tag})

customEnum : a 
             -> CustomCodec v String String a String
customEnum a  = custom a "" (\x -> x) (\ x _ -> x)

-- variant1 "A" A .A defautA codec
variant1 : comparable -> (arg -> v)
             -> (arg2 -> (common -> common))
             -> (common -> arg2)
             -> Codec arg arg2
             -> CustomCodec v comparable common 
                          ((arg -> common) -> a) x
             -> CustomCodec v comparable common a v
variant1 tag constr enc deco codec (CustomCodec c) =
    let dec = (\r -> r |> deco |> decoder codec |> constr) in
    CustomCodec {
        -- builder a pour type ((arg2 -> c2) -> r)
        encoder = c.encoder 
             (\ arg -> c.defaultCommon 
                |> c.setTag tag 
                |> enc (encoder codec arg)
                )
              
        
      , decoder = Dict.insert tag 
                  dec
                  c.decoder
      , defaultDecoder = dec
      , defaultCommon = c.defaultCommon
      , getTag = c.getTag
      , setTag = c.setTag
    }

variant2 : comparable -> (arg1 -> arg2 -> v)
             -> (arg1 -> arg2 -> (common -> common))
             -> (common -> arg1)
             -> (common -> arg2)
             -> CustomCodec v comparable common 
                          ((arg1 -> arg2 -> common) -> a) x
             -> CustomCodec v comparable common a v
variant2 tag constr enc dec1 dec2 (CustomCodec c) =
   let cc2 = {
              encoder = (\ f -> c.encoder (\ a b -> f (a,b)))
            , decoder = c.decoder
            , defaultDecoder = c.defaultDecoder
            , defaultCommon = c.defaultCommon
            , getTag = c.getTag
            , setTag = c.setTag
             }
   in
   variant1 tag (\ (a,b) -> constr a b)(\ (a,b) -> enc a b)
                (\ r -> (dec1 r, dec2 r))
                identity
                (CustomCodec cc2)
                

variant0 : comparable -> v
             -> CustomCodec v comparable common 
                          (common -> a) x
             -> CustomCodec v comparable common a v
variant0 tag constr (CustomCodec c) =
   let cc2 = {
              encoder = (\ f -> c.encoder (f ()))
            , decoder = c.decoder
            , defaultDecoder = c.defaultDecoder
            , defaultCommon = c.defaultCommon
            , getTag = c.getTag
            , setTag = c.setTag
             }
   in
   variant1 tag (\ _ -> constr)(\ _ -> Basics.identity)
                (\_ -> ())
                identity
                (CustomCodec cc2)
                



buildVariant : CustomCodec v comparable common 
                (v -> common) v -> 
                Codec v common
buildVariant (CustomCodec c) =
    Codec {
        encoder = c.encoder
      , decoder = \ r ->
           case Dict.get (c.getTag r) c.decoder of 
              Nothing -> c.defaultDecoder r
              Just f -> f r
    }
   

{-

Another way to encode custom types:
{ consA : Mayb argA, consB : Maybe argB, consC : Bool , ..}
consC is aconstructor with 0 argument

example:

  let splitMsg snapshot clear load modif v = 
           case v of 
               Snapshot arg -> snapshot arg
               ModifProtocol arg -> modif arg
               LoadProtocol arg -> load arg
               ClearProtocol arg -> clear arg
    in
    Codec.maybeCustom splitMsg 
    (
   \ snapshot clear load modif -> {snapshot = snapshot,
      clear = clear, load = load, modif = modif }
    )
    |> Codec.maybeVariant1 Snapshot .snapshot Format.LastVersion.graphInfoCodec
    |> Codec.maybeVariant1 ClearProtocol .clear clearCodec
    |> Codec.maybeVariant1 LoadProtocol .load loadCodec
    |> Codec.maybeVariant1 ModifProtocol .modif protocolModifCodec
    |> Codec.maybeBuildVariant defaultProtocolMsg

-}
type MaybeCustomCodec v r a b = MaybeCustomCodec
    { 
      encoder : (b -> r) ->  a
    , make : b
    , decoder : (r -> v) -> r -> v
    }

maybeCustom : a -> b -> MaybeCustomCodec v r a b
maybeCustom a make = 
   MaybeCustomCodec { encoder = always a
    , make = make
    , decoder = Basics.identity -- \ _ _ -> v
    }

maybeBuildVariant : v -> MaybeCustomCodec v r (v -> r) r -> Codec v r
maybeBuildVariant defaultV (MaybeCustomCodec c) =
    build 
    (c.encoder Basics.identity)
    (c.decoder (always defaultV))

maybeVariant0 : v -> (r -> Bool) -> MaybeCustomCodec v r (r -> a) (Bool -> b) 
         -> MaybeCustomCodec v r a b 
maybeVariant0 constr proj (MaybeCustomCodec c) =
  MaybeCustomCodec {
     decoder = \ f -> c.decoder (\ r -> if proj r then constr else f r)
   , make = c.make False
               -- f de type b -> r
   , encoder = \ f -> c.encoder 
                      -- g de type (Bool -> b)
                   (\g -> f (g False))
                   (f <| c.make True)
  }

maybeVariant1 : (x -> v) -> (r -> Maybe y) -> Codec x y ->
          MaybeCustomCodec v r ((x -> r) -> a) (Maybe y -> b) 
         -> MaybeCustomCodec v r a b 
maybeVariant1 constr proj cxy (MaybeCustomCodec c) =
  MaybeCustomCodec {
     decoder = \ f -> c.decoder 
        (\ r -> 
           case proj r of 
             Nothing -> f r
             Just y -> constr <| decoder cxy y
        )
   , make = c.make Nothing
               -- f de type b -> r
   , encoder = \ f -> c.encoder 
                      -- g de type (Maybe y -> b)
                   (\g -> f (g Nothing))
                   (f << c.make << Just << encoder cxy)
  }