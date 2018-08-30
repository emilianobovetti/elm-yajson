module YajsonTest exposing (at, compactStringify, encode, fromString, fromValue, index, jsonPrettyString, jsonString, jsonValue, member, prettyStringify, toList, yajsonValue)

import Expect
import Json.Encode as Encode
import Test exposing (Test, describe, test)
import Yajson exposing (Json)
import Yajson.Array
import Yajson.Object
import Yajson.Stringify


jsonString : String
jsonString =
    """{"str":"val","num":0,"bool":false,"null":null,"obj":{"k":"v"},"lst":[0,1]}"""


jsonPrettyString : String
jsonPrettyString =
    """
{ "str": "val"
, "num": 0
, "bool": false
, "null": null
, "obj":<space>
    { "k": "v"
    }
, "lst":<space>
    [ 0
    , 1
    ]
}
    """
        |> String.split "<space>"
        |> String.join " "
        |> String.trim


jsonValue : Encode.Value
jsonValue =
    Encode.object
        [ ( "str", Encode.string "val" )
        , ( "num", Encode.float 0 )
        , ( "bool", Encode.bool False )
        , ( "null", Encode.null )
        , ( "obj", Encode.object [ ( "k", Encode.string "v" ) ] )
        , ( "lst", Encode.list Encode.int [ 0, 1 ] )
        ]


yajsonKeyList : List ( String, Json )
yajsonKeyList =
    [ ( "str", Yajson.String "val" )
    , ( "num", Yajson.Number 0 )
    , ( "bool", Yajson.Bool False )
    , ( "null", Yajson.Null )
    , ( "obj", Yajson.Object [ ( "k", Yajson.String "v" ) ] )
    , ( "lst", Yajson.Array [ Yajson.Number 0, Yajson.Number 1 ] )
    ]


yajsonValue : Json
yajsonValue =
    Yajson.Object yajsonKeyList


yajsonRevValue : Json
yajsonRevValue =
    Yajson.Object (List.reverse yajsonKeyList)


fromValue : Test
fromValue =
    describe "Yajson.fromValue"
        [ test "should decode json from Json.Encode.Value" <|
            \() ->
                Expect.equal (Ok yajsonRevValue) (Yajson.fromValue jsonValue)
        ]


fromString : Test
fromString =
    describe "Yajson.fromString"
        [ test "should decode json from string" <|
            \() ->
                Expect.equal (Ok yajsonRevValue) (Yajson.fromString jsonString)
        ]


encode : Test
encode =
    describe "Yajson.encode"
        [ test "should match the Json.Encode.Value" <|
            \() ->
                Expect.equal jsonValue (Yajson.encode yajsonValue)
        ]


compactStringify : Test
compactStringify =
    describe "Yajson.Stringify.compact"
        [ test "should match its string representation" <|
            \() ->
                Expect.equal jsonString (Yajson.Stringify.compact yajsonValue)
        ]


prettyStringify : Test
prettyStringify =
    describe "Yajson.Stringify.pretty"
        [ test "should match its string representation" <|
            \() ->
                Expect.equal jsonPrettyString (Yajson.Stringify.pretty yajsonValue)
        ]


member : Test
member =
    describe "Yajson.member"
        [ test "should get object property by its key" <|
            \() ->
                Expect.equal
                    (Just <| Yajson.String "value")
                    (Yajson.member "key" <| Yajson.Object.ofString [ ( "key", "value" ) ])
        , test "should return Nothing if key doesn't exist" <|
            \() ->
                Expect.equal
                    Nothing
                    (Yajson.member "key" <| Yajson.Object [])
        , test "should return Nothing if json value is not an object" <|
            \() ->
                Expect.equal
                    Nothing
                    (Yajson.member "key" Yajson.Null)
        ]


at : Test
at =
    describe "Yajson.at"
        [ test "should get nested object property" <|
            \() ->
                Expect.equal
                    (Just <| Yajson.String "value")
                    (Yajson.at [ "1", "2" ] <|
                        Yajson.Object
                            [ ( "1", Yajson.Object.ofString [ ( "2", "value" ) ] ) ]
                    )
        , test "should behave as 'member' if key list contains one key" <|
            \() ->
                let
                    obj =
                        Yajson.Object.ofInt [ ( "1", 1 ) ]
                in
                Expect.equal
                    (Yajson.member "1" obj)
                    (Yajson.at [ "1" ] obj)
        , test "should return Nothing if key list is empty" <|
            \() ->
                Expect.equal Nothing (Yajson.at [] <| Yajson.Object.ofInt [ ( "1", 1 ) ])
        ]


index : Test
index =
    describe "Yajson.index"
        [ test "should return first array element with index = 0" <|
            \() ->
                Expect.equal (Just <| Yajson.Number 1) (Yajson.index 0 <| Yajson.Array.ofInt [ 1, 2, 3 ])
        , test "should return second array element with index = 1" <|
            \() ->
                Expect.equal (Just <| Yajson.Number 2) (Yajson.index 1 <| Yajson.Array.ofInt [ 1, 2, 3 ])
        , test "should return third array element with index = 2" <|
            \() ->
                Expect.equal (Just <| Yajson.Number 3) (Yajson.index 2 <| Yajson.Array.ofInt [ 1, 2, 3 ])
        , test "should return last array element with index = -1" <|
            \() ->
                Expect.equal (Just <| Yajson.Number 3) (Yajson.index -1 <| Yajson.Array.ofInt [ 1, 2, 3 ])
        , test "should return penultimate array element with index = -2" <|
            \() ->
                Expect.equal (Just <| Yajson.Number 2) (Yajson.index -2 <| Yajson.Array.ofInt [ 1, 2, 3 ])
        , test "should return Nothing if a positive index is out of bounds" <|
            \() ->
                Expect.equal Nothing (Yajson.index 3 <| Yajson.Array.ofInt [ 1, 2, 3 ])
        , test "should return Nothing if a negative index is out of bounds" <|
            \() ->
                Expect.equal Nothing (Yajson.index -4 <| Yajson.Array.ofInt [ 1, 2, 3 ])
        ]


toList : Test
toList =
    describe "Yajson.toList"
        [ test "should return a list of json values if the json is an array" <|
            \() ->
                Expect.equal
                    [ Yajson.Number 1, Yajson.Number 2, Yajson.Number 3 ]
                    (Yajson.toList <| Yajson.Array.ofInt [ 1, 2, 3 ])
        , test "should return an empty list if the json is not an array" <|
            \() ->
                Expect.equal [] (Yajson.toList Yajson.Null)
        ]
