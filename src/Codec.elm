-- inspired by https://package.elm-lang.org/packages/miniBill/elm-codec/latest/
module Codec exposing
    ( Codec, build
    , identity 
    , decoder 
    , encoder
    , enum
    , list
    , ObjectCodec, object, compose, composite
    , bothFields, buildObject 
    )


import List.Extra as List



-- DEFINITION


{-| A value that knows how to encode and decode JSON values.
-}
type Codec a b
    = Codec
        { encoder : a -> b
        , decoder : b -> a
        }

-- swap : Codec a b -> Codec b a
-- swap (Codec {encoder, decoder}) = Codec {encoder = decoder, decoder = encoder}

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


{-
{-| Convert a value into a prettified JSON string. The first argument specifies
the amount of indentation in the result string.
-}
encodeToString : Int -> Codec a -> a -> String
encodeToString indentation codec =
    encoder codec >> JE.encode indentation


{-| Convert a value into a Javascript `Value`.
-}
encodeToValue : Codec a -> a -> Value
encodeToValue codec =
    encoder codec

-}

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



{-| `Codec` for a fixed list of Elm values.

This can be used for custom types, if they are really simple:

    type Semaphore = Red | Yellow | Green

    semaphoreCodec : Codec Semaphore
    semaphoreCodec =
        enum Codec.string
            [ ("Red", Red)
            , ("Yellow", Yellow)
            , ("Green", Green)
            ]

    encodeToString 0 semaphoreCodec Red
    --> "\"Red\""
    decodeString semaphoreCodec "\"Red\""
    --> Ok Red

    type Count = One | Two | Three

    countCodec : Codec Count
    countCodec =
        enum Codec.int
            [ (1, One)
            , (2, Two)
            , (3, Three)
            ]

    encodeToString 0 countCodec Two
    --> "2"
    decodeString countCodec "2"
    --> Ok Two

    incompleteCodec : Codec Count
    incompleteCodec =
        enum Codec.int
            [ (1, One)
            , (2, Two)
            ]

    encodeToString 0 incompleteCodec Three
    --> "null"

    decodeString incompleteCodec "3" |> Result.mapError (\_ -> "...")
    --> Err "..."

-}
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


{-| Represents an optional value.

This is encoded as `null` when the input is `Nothing`, and the same as `x` when `Just x`.

If the decoding using the inner `Codec` fails, it will _succeed_ with `Nothing`. If you want it to fail use `nullable` instead.

    encodeToString 0 (maybe int) (Just 3)
    --> "3"
    encodeToString 0 (maybe int) Nothing
    --> "null"

    decodeString (maybe int) "3"
    --> Ok (Just 3)
    decodeString (maybe int) "null"
    --> Ok Nothing
    decodeString (maybe int) "\"hello\""
    --> Ok Nothing

-}
{-
maybe : Codec a -> Codec (Maybe a)
maybe codec =
    Codec
        { decoder = JD.maybe <| decoder codec
        , encoder =
            \v ->
                case v of
                    Nothing ->
                        JE.null

                    Just x ->
                        encoder codec x
        }
        -}


{- | Represents an optional value.

This is encoded as `null` when the input is `Nothing`, and the same as `x` when `Just x`.

When decoding, it decodes `null` to `Nothing`. Otherwise, if the decoding using the inner `Codec` fails, it will fail. If you want it to succeed with `Nothing` use `maybe` instead.

    encodeToString 0 (nullable int) (Just 3)
    --> "3"
    encodeToString 0 (nullable int) Nothing
    --> "null"

    decodeString (nullable int) "3"
    --> Ok (Just 3)
    decodeString (nullable int) "null"
    --> Ok Nothing
    decodeString (nullable int) "\"hello\"" |> Result.mapError (\_ -> "...")
    --> Err "..."

-}
{-
nullable : Codec a -> Codec (Maybe a)
nullable codec =
    Codec
        { decoder = JD.nullable <| decoder codec
        , encoder =
            \v ->
                case v of
                    Nothing ->
                        JE.null

                    Just x ->
                        encoder codec x
        }
-}

{- | `Codec` between a JSON array and an Elm `List`.
-}
list : Codec a b -> Codec (List a) (List b)
list =
    composite List.map List.map


{-| `Codec` between a JSON array and an Elm `Array`.
-}
{-
array : Codec a -> Codec (Array a)
array =
    composite JE.array JD.array
    -}


{- | `Codec` between a JSON object and an Elm `Dict`.
-}
{-
dict : Codec a -> Codec (Dict String a)
dict =
    composite
        (\e -> JE.object << Dict.toList << Dict.map (\_ -> e))
        JD.dict
-}


{-

{-| `Codec` between a JSON array and an Elm `Set`.
-}
set : Codec comparable -> Codec (Set comparable)
set =
    composite
        (\e -> JE.list e << Set.toList)
        (JD.map Set.fromList << JD.list)


{-| `Codec` between a JSON array of length 2 and an Elm `Tuple`.
-}
tuple : Codec a -> Codec b -> Codec ( a, b )
tuple m1 m2 =
    Codec
        { encoder =
            \( v1, v2 ) ->
                JE.list identity
                    [ encoder m1 v1
                    , encoder m2 v2
                    ]
        , decoder =
            JD.map2
                (\a b -> ( a, b ))
                (JD.index 0 <| decoder m1)
                (JD.index 1 <| decoder m2)
        }


{-| `Codec` between a JSON array of length 3 and an Elm triple.
-}
triple : Codec a -> Codec b -> Codec c -> Codec ( a, b, c )
triple m1 m2 m3 =
    Codec
        { encoder =
            \( v1, v2, v3 ) ->
                JE.list identity
                    [ encoder m1 v1
                    , encoder m2 v2
                    , encoder m3 v3
                    ]
        , decoder =
            JD.map3
                (\a b c -> ( a, b, c ))
                (JD.index 0 <| decoder m1)
                (JD.index 1 <| decoder m2)
                (JD.index 2 <| decoder m3)
        }


{-| `Codec` for `Result` values.
-}
result : Codec error -> Codec value -> Codec (Result error value)
result errorCodec valueCodec =
    custom
        (\ferr fok v ->
            case v of
                Err err ->
                    ferr err

                Ok ok ->
                    fok ok
        )
        |> variant1 "Err" Err errorCodec
        |> variant1 "Ok" Ok valueCodec
        |> buildCustom

-}

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

    pointCodec : Codec Point
    pointCodec =
        Codec.object Point
            |> Codec.field "x" .x Codec.float
            |> Codec.field "y" .y Codec.float
            |> Codec.buildObject

    type alias Polar =
        { r : Float
        , theta : Float
        }

     type alias Point2 =
        { x2 : Float
        , y2 : Float
        }

    pointCodec : Codec Point Point2
    pointCodec =
        Codec.object Point Point2
            |> Codec.bothFields .x .x2
            |> Codec.bothFields .y .y2
            |> Codec.buildObject


    pointCodec : Codec Point Polar
    pointCodec =
        Codec.object Point Polar
            |> Codec.field1 rThetaToX
            |> Codec.field1 rThetaToY
            |> Codec.buildObject

Example without constructor:

    pointCodec : Codec { x : Int, y : Bool }
    pointCodec =
        Codec.object (\x y -> { x = x, y = y })
            |> Codec.field "x" .x Codec.int
            |> Codec.field "y" .y Codec.bool
            |> Codec.buildObject

-}
object : b1 -> b2 -> ObjectCodec a1 a2 b1 b2
object b1 b2 =
    ObjectCodec
        { 
          encoder = always b2
        , decoder = always b1
        }
{-
bothFields : (a1 -> f2) -> (a2 -> f1)
    ->  ObjectCodec a1 a2 (f1 -> b1) (f2 -> b2) 
    ->  ObjectCodec a1 a2 b1 b2
bothFields getter1 getter2 (ObjectCodec ocodec) =
    ObjectCodec {
        encoder = \ a1 -> ocodec.encoder a1 (getter1 a1)
      , decoder = \ a2 -> ocodec.decoder a2 (getter2 a2)
    }
-}
bothFields : (a1 -> f1) -> (a2 -> f2)
    -> Codec f1 f2
    ->  ObjectCodec a1 a2 (f1 -> b1) (f2 -> b2) 
    ->  ObjectCodec a1 a2 b1 b2
bothFields getter1 getter2 cod (ObjectCodec ocodec) =
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

