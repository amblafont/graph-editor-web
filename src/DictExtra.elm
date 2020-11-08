module DictExtra exposing (insertOrRemove)

import Dict exposing (Dict)

insertOrRemove : comparable -> Maybe v -> Dict comparable v -> Dict comparable v
insertOrRemove k v d =
    case v of
        Just val -> Dict.insert k val d
        Nothing -> Dict.remove k d
