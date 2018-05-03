module ArrayTest exposing (..)

import Test exposing (Test, describe, test)
import Yajson.Array as Array
import Yajson
import Expect


suite : Test
suite =
    describe "Yajson.Array"
        [ describe "ofString"
            [ test "should create an array with string values" <|
                \() ->
                    Expect.equal
                        (Yajson.Array [ Yajson.String "1", Yajson.String "2" ])
                        (Array.ofString [ "1", "2" ])
            ]
        , describe "ofFloat"
            [ test "should create an array with float values" <|
                \() ->
                    Expect.equal
                        (Yajson.Array [ Yajson.Number 1, Yajson.Number 2 ])
                        (Array.ofFloat [ 1, 2 ])
            ]
        , describe "ofInt"
            [ test "should create an array with int values" <|
                \() ->
                    Expect.equal
                        (Yajson.Array [ Yajson.Number 1, Yajson.Number 2 ])
                        (Array.ofInt [ 1, 2 ])
            ]
        , describe "ofBool"
            [ test "should create an array with boolean values" <|
                \() ->
                    Expect.equal
                        (Yajson.Array [ Yajson.Bool True, Yajson.Bool False ])
                        (Array.ofBool [ True, False ])
            ]
        , describe "map"
            [ test "should map json array values" <|
                \() ->
                    let
                        incNum json =
                            case json of
                                Yajson.Number n ->
                                    n + 1

                                _ ->
                                    0
                    in
                        Expect.equal
                            [ 1, 2, 3 ]
                            (Array.map incNum <| Array.ofInt [ 0, 1, 2 ])
            ]
        ]
