module Yajson.Array
    exposing
        ( map
        , filterMap
        , ofString
        , ofFloat
        , ofInt
        , ofBool
        )

{-| Module for array specific helpers.


# Map

@docs map, filterMap


# Creation

@docs ofString, ofFloat, ofInt, ofBool

-}

import Yajson exposing (Json(..))


{-| Maps json arrays. Returns an empty list if
json value is not an array.

    json : Json
    json =
        Array [ Number 0, Bool False ]

    map Yajson.Stringify.compact json == [ "0", "false" ]

-}
map : (Json -> a) -> Json -> List a
map fn json =
    filterMap (fn >> Just) json


{-| Given a funcion `fn : Json -> Maybe a` maps
a json array, removes `Nothing`s and returns a list.

If the json value is not an array returns an empty list.

    inc : Json -> Maybe Float
    inc json =
        case json of
            Number n ->
                Just (n + 1)

            _ ->
                Nothing

    json : Json
    json =
        Array [ Number 0, Number 1, Bool True ]

    filterMap inc json == [ 1, 2 ]

-}
filterMap : (Json -> Maybe a) -> Json -> List a
filterMap fn json =
    case json of
        Array lst ->
            List.filterMap fn lst

        _ ->
            []


{-| Wraps a list of strings in a json value.

    ofString [ "hello", "world" ]
        == Array [ String "hello", String "world" ]

-}
ofString : List String -> Json
ofString lst =
    Array (List.map Yajson.String lst)


{-| Wraps a list of floats in a json value.

    ofFloat [ 3.14, 2.718 ]
        == Array [ Number 3.14, Number 2.718 ]

-}
ofFloat : List Float -> Json
ofFloat lst =
    Array (List.map Yajson.Number lst)


{-| Wraps a list of ints in a json value.

    ofInt [ 3, 4 ]
        == Array [ Number 3, Number 4 ]

-}
ofInt : List Int -> Json
ofInt lst =
    Array (List.map (toFloat >> Yajson.Number) lst)


{-| Wraps a list of booleans in a json value.

    ofBool [ False, True ]
        == Array [ Bool False, Bool True ]

-}
ofBool : List Bool -> Json
ofBool lst =
    Array (List.map Yajson.Bool lst)
