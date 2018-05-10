module Yajson
    exposing
        ( Json
            ( Object
            , Array
            , String
            , Number
            , Bool
            , Null
            )
        , decoder
        , fromValue
        , fromString
        , encode
        , member
        , at
        , index
        , flatten
        , filterMember
        , filterAt
        , filterIndex
        , filterAssoc
        , filterList
        , filterBool
        , filterInt
        , filterFloat
        , filterString
        , toMaybeAssoc
        , toAssoc
        , toMaybeList
        , toList
        , toString
        , toFloat
        , toInt
        , toBool
        )

{-| Yet another JSON library
inspired by OCaml [Yojson](https://github.com/mjambon/yojson).


# Data structure

@docs Json


# Decoding

@docs decoder, fromValue, fromString


# Encoding

@docs encode


# Util

@docs member, at, index, flatten


# Filter

@docs filterMember, filterAt, filterIndex, filterAssoc, filterList
@docs filterBool, filterInt, filterFloat, filterString


# Conversion

@docs toMaybeAssoc, toAssoc, toMaybeList, toList
@docs toString, toFloat, toInt, toBool

-}

import Json.Decode as Decode
import Json.Encode as Encode


{-| JSON representation with Elm's union types.
`String`s and `Json.Encode.Value`s can both be converted in
this data structure or can be used to represent a JSON in Elm.

Note in this example I'm using the infix [=>](Yajson-Infix) operator
instead of standard tuple `("name", String "Fred")`.

    person : Json
    person =
        Object
            [ "name" => String "Fred"
            , "age" => Number 20
            , "favorite_colors"
                => Array [ String "red", String "green" ]
            ]

-}
type Json
    = Object (List ( String, Json ))
    | Array (List Json)
    | String String
    | Number Float
    | Bool Bool
    | Null


{-| Decoder for converting values in `Yajson.Json` data.

    Json.Decode.decodeValue decoder jsonValue

-}
decoder : Decode.Decoder Json
decoder =
    let
        objectDecoder : Decode.Decoder (List ( String, Json ))
        objectDecoder =
            Decode.keyValuePairs (Decode.lazy (\() -> decoder))
                |> Decode.map List.reverse

        arrayDecoder : Decode.Decoder (List Json)
        arrayDecoder =
            Decode.list (Decode.lazy (\() -> decoder))
    in
        Decode.oneOf
            [ objectDecoder |> Decode.map Object
            , arrayDecoder |> Decode.map Array
            , Decode.string |> Decode.map String
            , Decode.float |> Decode.map Number
            , Decode.bool |> Decode.map Bool
            , Decode.null Null
            ]


{-| Shortcut for `Json.Decode.decodeValue decoder`.

    fromValue jsonValue

-}
fromValue : Encode.Value -> Result String Json
fromValue val =
    Decode.decodeValue decoder val


{-| Shortcut for `Json.Decode.decodeString decoder`.

    fromString """{ "hello": "world" }"""

-}
fromString : String -> Result String Json
fromString raw =
    Decode.decodeString decoder raw


{-| Encode `Json` in `Json.Encode.Value`.

    encode (String "hello there")

-}
encode : Json -> Encode.Value
encode json =
    case json of
        Object assoc ->
            assoc
                |> List.map (Tuple.mapSecond encode)
                |> Encode.object

        Array lst ->
            lst
                |> List.map encode
                |> Encode.list

        String str ->
            Encode.string str

        Number num ->
            Encode.float num

        Bool bool ->
            Encode.bool bool

        Null ->
            Encode.null


{-| Returns the value associated with the given
key in a json object, or `Nothing` if the key
isn't present in object or the json value isn't an object.

    member "anything" Null == Nothing

    member "a_key" (Object [ "a_key" => String "a_value" ])
        == Just (String "a_value")

-}
member : String -> Json -> Maybe Json
member key json =
    case json of
        Object assoc ->
            assoc
                |> List.filter (Tuple.first >> (==) key)
                |> List.head
                |> Maybe.map Tuple.second

        _ ->
            Nothing


{-| Works like [`member`](#member), but with nested objects.

    nested : Json
    nested =
        """{ "foo": { "bar" : "baz" } }"""
            |> fromString
            |> Result.withDefault Null

    at [ "foo", "bar" ] Null == Nothing
    at [ "foo", "baz" ] nested == Nothing
    at [ "foo", "bar" ] nested == Just (String "baz")

-}
at : List String -> Json -> Maybe Json
at keyLst json =
    case keyLst of
        [] ->
            Nothing

        [ key ] ->
            member key json

        key :: keys ->
            json
                |> member key
                |> Maybe.andThen (at keys)


{-| Returns the value at index `i` in a json array.
Indices start at 0 and negative ones count from the end,
so `-1` is the last element.

    array : Json
    array =
        Array [ Number 1, Number 2, Number 3 ]

    index 0 Null == Nothing
    index 0 (Array []) == Nothing
    index 1 array == Just (Number 2)
    index -1 array == Just (Number 3)

-}
index : Int -> Json -> Maybe Json
index idx json =
    case json of
        Array lst ->
            if idx < 0 then
                List.reverse lst
                    |> List.drop (-idx - 1)
                    |> List.head
            else
                lst
                    |> List.drop idx
                    |> List.head

        _ ->
            Nothing


{-| Expects a list of json array and returns
all their elements as a single list.

    ints : List Int
    ints =
        [ Array [ Number 1, Number 2.3 ]
        , Array [ Number 3, Bool False ]
        , Array [ String "hey", Number 4 ]
        ]
            |> flatten
            |> filterInt

    ints == [1,3,4]

-}
flatten : List Json -> List Json
flatten lst =
    List.concat (filterList lst)


{-| Expects a list of json objects and returns
all the fields with the given name.

    joe : Json
    joe =
        """{ "name": "Joe" }"""
            |> fromString
            |> Result.withDefault Null

    jill : Json
    jill =
        """{ "name": "Jill" }"""
            |> fromString
            |> Result.withDefault Null

    names : List String
    names =
        [ joe, jill ]
            |> filterMember "name"
            |> filterString

    names == [ "Joe", "Jill" ]

-}
filterMember : String -> List Json -> List Json
filterMember key lst =
    List.filterMap (member key) lst


{-| Works like [`filterMember`](#filterMember),
but with nested objects.

    joe : Json
    joe =
        """
            { "name": "Joe"
            , "father": { "name": "Bob" }
            }
        """
            |> fromString
            |> Result.withDefault Null

    jill : Json
    jill =
        """
            { "name": "Jill"
            , "father": { "name": "Vincent" }
            }
        """
            |> fromString
            |> Result.withDefault Null

    fatherNames : List String
    fatherNames =
        [ joe, jill ]
            |> filterAt [ "father", "name" ]
            |> filterString

    fatherNames == [ "Bob", "Vincent" ]

-}
filterAt : List String -> List Json -> List Json
filterAt keyLst lst =
    List.filterMap (at keyLst) lst


{-| Expects a list of json array and returns
all their elements existing at the given position.

    additives : Json
    additives =
        """[ "red", "green", "blue" ]"""
            |> fromString
            |> Result.withDefault Null

    subtractives : Json
    subtractives =
        """[ "cyan", "magenta", "yellow" ]"""
            |> fromString
            |> Result.withDefault Null

    lasts : List String
    lasts =
        [ additives, subtractives ]
            |> filterIndex -1
            |> filterString

    lasts == [ "blue", "yellow" ]

-}
filterIndex : Int -> List Json -> List Json
filterIndex idx lst =
    List.filterMap (index idx) lst


{-| Expects a list of json objects and unwraps them.

    min : Maybe Int
    min =
        [ Object [ "key_3" => Number 10 ]
        , Object [ "key_1" => Number -1 ]
        , Object [ "key_2" => Number 11 ]
        ]
            |> filterAssoc
            |> List.concatMap (List.map Tuple.second)
            |> filterInt
            |> List.minimum

    min == Just -1

-}
filterAssoc : List Json -> List (List ( String, Json ))
filterAssoc lst =
    List.filterMap toMaybeAssoc lst


{-| Expects a list of json array and unwraps them.

    firsts : List Int
    firsts =
        [ Array [ Number 1, Number 2 ]
        , Array [ Number 3, Number 4 ]
        , Array [ String "hey", Number 5 ]
        ]
            |> filterList
            |> List.filterMap List.head
            |> filterInt

    firsts == [1,3]

-}
filterList : List Json -> List (List Json)
filterList lst =
    List.filterMap toMaybeList lst


{-| Expects a list of json strings and unwraps them.

    filterString [ Number 0, String "str" ]
        == [ "str" ]

-}
filterString : List Json -> List String
filterString lst =
    List.filterMap toString lst


{-| Expects a list of json floats and unwraps them.

    filterFloat [ Bool True, Number 1, Number 1.2 ]
        == [ 1, 1.2 ]

-}
filterFloat : List Json -> List Float
filterFloat lst =
    List.filterMap toFloat lst


{-| Expects a list of json ints and unwraps them.

    filterInt [ Number 1, Number 2, Number 1.2 ]
        == [ 1, 2 ]

-}
filterInt : List Json -> List Int
filterInt lst =
    List.filterMap toInt lst


{-| Expects a list of json booleans and unwraps them.

    filterBool [ Bool False, Bool True ]
        == [ False, True ]

-}
filterBool : List Json -> List Bool
filterBool lst =
    List.filterMap toBool lst


{-| Extracts object representation from a `Json`
and wraps it in a `Maybe`. If the json isn't an object
returns `Nothing`.

    toMaybeAssoc Null == Nothing

    toMaybeAssoc (Object [ "key" => String "value" ])
        == Just [ "key" => String "value" ]

-}
toMaybeAssoc : Json -> Maybe (List ( String, Json ))
toMaybeAssoc json =
    case json of
        Object assoc ->
            Just assoc

        _ ->
            Nothing


{-| Extracts object representation from a `Json`.
If the json value isn't an object returns an empty list.

    toAssoc Null == []

    toAssoc (Object [ "key" => String "value" ])
        == [ "key" => String "value" ]

-}
toAssoc : Json -> List ( String, Json )
toAssoc json =
    json
        |> toMaybeAssoc
        |> Maybe.withDefault []


{-| Extracts array representation from a `Json`
and wraps it in a `Maybe`. If the json isn't an array
returns `Nothing`.

    toMaybeList Null == Nothing

    toMaybeList (Array [ Number 1, Number 2 ])
        == Just [ Number 1, Number 2 ]

-}
toMaybeList : Json -> Maybe (List Json)
toMaybeList json =
    case json of
        Array lst ->
            Just lst

        _ ->
            Nothing


{-| Extracts array representation from a `Json`.
If the json value isn't an array returns an empty list.

    toList Null == []

    toList (Array [ Number 1, Number 2 ])
        == [ Number 1, Number 2 ]

-}
toList : Json -> List Json
toList json =
    json
        |> toMaybeList
        |> Maybe.withDefault []


{-| Extracts a string from a json, returns `Nothing` if
the value doesn't contain a string.

    toString Null == Nothing
    toString (String "hey") == Just "hey"

-}
toString : Json -> Maybe String
toString json =
    case json of
        String str ->
            Just str

        _ ->
            Nothing


{-| Extracts a float from a json, returns `Nothing` if
the value doesn't contain a float.

    toFloat Null == Nothing
    toFloat (Number 1) == Just 1

-}
toFloat : Json -> Maybe Float
toFloat json =
    case json of
        Number num ->
            Just num

        _ ->
            Nothing


{-| Extracts an int from a json, returns `Nothing` if
the value doesn't contain an int.

    toInt (Number 0.1) == Nothing
    toInt (Number 1) == Just 1

-}
toInt : Json -> Maybe Int
toInt json =
    json
        |> toFloat
        |> Maybe.map Basics.toString
        |> Maybe.map String.toInt
        |> Maybe.andThen Result.toMaybe


{-| Extracts a boolean from a json, returns `Nothing` if
the value doesn't contain a boolean.

    toBool Null == Nothing
    toBool (Bool False) == Just False

-}
toBool : Json -> Maybe Bool
toBool json =
    case json of
        Bool bool ->
            Just bool

        _ ->
            Nothing
