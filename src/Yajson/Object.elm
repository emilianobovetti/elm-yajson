module Yajson.Object
    exposing
        ( keys
        , values
        , map
        , mapKeys
        , mapValues
        , filterMap
        , filterMapKeys
        , filterMapValues
        , ofString
        , ofFloat
        , ofInt
        , ofBool
        )

{-| Module for object specific helpers.


# Access

@docs keys, values


# Map

@docs map, mapKeys, mapValues
@docs filterMap, filterMapKeys, filterMapValues


# Creation

@docs ofString, ofFloat, ofInt, ofBool

-}

import Yajson exposing (Json(..))


{-| Takes a json object and returns its keys as list of strings.
Returns an empty list if the value isn't an object.

    keys (ofInt [ ( "I", 1 ), ( "II", 2 ), ( "III", 3 ) ])
        == [ "I", "II", "III" ]

-}
keys : Json -> List String
keys json =
    case json of
        Object assoc ->
            List.map Tuple.first assoc

        _ ->
            []


{-| Takes a json object and returns its values as list of strings.
Returns an empty list if the value isn't an object.

    values (ofInt [ ( "I", 1 ), ( "II", 2 ), ( "III", 3 ) ])
        == [ Number 1, Number 2, Number 3 ]

-}
values : Json -> List Json
values json =
    case json of
        Object assoc ->
            List.map Tuple.second assoc

        _ ->
            []


{-| Maps a json object.
-}
map : (String -> Json -> a) -> Json -> List a
map fn json =
    filterMap (\k v -> Just (fn k v)) json


{-| Like [`map`](#map), but maps only the keys.
-}
mapKeys : (String -> a) -> Json -> List ( a, Json )
mapKeys fn json =
    map (\k v -> ( fn k, v )) json


{-| Like [`map`](#map), but maps only the values.
-}
mapValues : (Json -> a) -> Json -> List ( String, a )
mapValues fn json =
    map (\k v -> ( k, fn v )) json


{-| Given a funcion `fn : String -> Json -> Maybe a`
maps a json object, removes `Nothing`s and returns a list.

If the json value is not an object returns an empty list.

-}
filterMap : (String -> Json -> Maybe a) -> Json -> List a
filterMap fn json =
    case json of
        Object assoc ->
            List.filterMap (\( k, v ) -> fn k v) assoc

        _ ->
            []


{-| Like [`filterMap`](#filterMap), but `fn` maps only the keys.
-}
filterMapKeys : (String -> Maybe a) -> Json -> List ( a, Json )
filterMapKeys fn json =
    filterMap (\k1 v -> k1 |> fn >> Maybe.map (\k2 -> ( k2, v ))) json


{-| Like [`filterMap`](#filterMap), but `fn` maps only the values.
-}
filterMapValues : (Json -> Maybe a) -> Json -> List ( String, a )
filterMapValues fn json =
    filterMap (\k v1 -> v1 |> fn >> Maybe.map (\v2 -> ( k, v2 ))) json


{-| Wraps a list of string pairs in a json object.

    ofString [ ( "name", "value" ) ]
        == Object [ ( "name", String "value" ) ]

-}
ofString : List ( String, String ) -> Json
ofString lst =
    Object (List.map (Tuple.mapSecond Yajson.String) lst)


{-| Wraps a list of pairs of string and float in a json object.

    ofFloat [ ( "pi", 3.14 ), ( "e", 2.718 ) ]
        == Object [ ( "pi", Number 3.14 ), ( "e", Number 2.718 ) ]

-}
ofFloat : List ( String, Float ) -> Json
ofFloat lst =
    Object (List.map (Tuple.mapSecond Yajson.Number) lst)


{-| Wraps a list of pairs of string and int in a json object.

    ofInt [ ( "I", 1 ), ( "II", 2 ) ]
        == Object [ ( "I", Number 1 ), ( "II", Number 2 ) ]

-}
ofInt : List ( String, Int ) -> Json
ofInt lst =
    Object (List.map (Tuple.mapSecond (toFloat >> Yajson.Number)) lst)


{-| Wraps a list of pairs of string and bool in a json object.

    ofBool [ ( "hot", True ), ( "cold", False ) ]
        == Object [ ( "hot", Bool True ), ( "cold", Bool False ) ]

-}
ofBool : List ( String, Bool ) -> Json
ofBool lst =
    Object (List.map (Tuple.mapSecond Yajson.Bool) lst)
