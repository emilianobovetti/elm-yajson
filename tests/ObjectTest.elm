module ObjectTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Yajson
import Yajson.Object


suite : Test
suite =
    describe "Yajson.Object"
        [ describe "ofString"
            [ test "should create an object with string values" <|
                \() ->
                    Expect.equal
                        (Yajson.Object
                            [ ( "1", Yajson.String "1" )
                            , ( "2", Yajson.String "2" )
                            ]
                        )
                        (Yajson.Object.ofString
                            [ ( "1", "1" )
                            , ( "2", "2" )
                            ]
                        )
            ]
        , describe "ofFloat"
            [ test "should create an object with float values" <|
                \() ->
                    Expect.equal
                        (Yajson.Object
                            [ ( "1", Yajson.Number 1 )
                            , ( "2", Yajson.Number 2 )
                            ]
                        )
                        (Yajson.Object.ofFloat
                            [ ( "1", 1 )
                            , ( "2", 2 )
                            ]
                        )
            ]
        , describe "ofInt"
            [ test "should create an object with int values" <|
                \() ->
                    Expect.equal
                        (Yajson.Object
                            [ ( "1", Yajson.Number 1 )
                            , ( "2", Yajson.Number 2 )
                            ]
                        )
                        (Yajson.Object.ofInt
                            [ ( "1", 1 )
                            , ( "2", 2 )
                            ]
                        )
            ]
        , describe "ofBool"
            [ test "should create an object with int boolean" <|
                \() ->
                    Expect.equal
                        (Yajson.Object
                            [ ( "1", Yajson.Bool True )
                            , ( "2", Yajson.Bool False )
                            ]
                        )
                        (Yajson.Object.ofBool
                            [ ( "1", True )
                            , ( "2", False )
                            ]
                        )
            ]
        , describe "keys"
            [ test "should return object keys" <|
                \() ->
                    Expect.equal
                        [ "1", "2", "3" ]
                        (Yajson.Object.keys <|
                            Yajson.Object.ofInt
                                [ ( "1", 1 )
                                , ( "2", 2 )
                                , ( "3", 3 )
                                ]
                        )
            ]
        , describe "values"
            [ test "should return object values" <|
                \() ->
                    Expect.equal
                        [ Yajson.Number 1, Yajson.Number 2, Yajson.Number 3 ]
                        (Yajson.Object.values <|
                            Yajson.Object.ofInt
                                [ ( "1", 1 )
                                , ( "2", 2 )
                                , ( "3", 3 )
                                ]
                        )
            ]
        , describe "map"
            [ test "should map json object keys and values" <|
                \() ->
                    let
                        incKeyVal key val =
                            case ( String.toInt key, val ) of
                                ( Just keyNum, Yajson.Number valNum ) ->
                                    ( String.fromInt (keyNum + 1), Yajson.Number (valNum + 1) )

                                _ ->
                                    ( key, val )
                    in
                    Expect.equal
                        [ ( "1", Yajson.Number 1 ), ( "2", Yajson.Number 2 ) ]
                        (Yajson.Object.map incKeyVal <|
                            Yajson.Object.ofInt [ ( "0", 0 ), ( "1", 1 ) ]
                        )
            ]
        , describe "mapKeys"
            [ test "should map json object keys" <|
                \() ->
                    let
                        incKey key =
                            case String.toInt key of
                                Just keyNum ->
                                    String.fromInt (keyNum + 1)

                                _ ->
                                    key
                    in
                    Expect.equal
                        [ ( "1", Yajson.Number 1 ), ( "2", Yajson.Number 2 ) ]
                        (Yajson.Object.mapKeys incKey <|
                            Yajson.Object.ofInt [ ( "0", 1 ), ( "1", 2 ) ]
                        )
            ]
        , describe "mapValues"
            [ test "should map json object values" <|
                \() ->
                    let
                        incVal val =
                            case val of
                                Yajson.Number num ->
                                    Yajson.Number (num + 1)

                                _ ->
                                    val
                    in
                    Expect.equal
                        [ ( "1", Yajson.Number 1 ), ( "2", Yajson.Number 2 ) ]
                        (Yajson.Object.mapValues incVal <|
                            Yajson.Object.ofInt [ ( "1", 0 ), ( "2", 1 ) ]
                        )
            ]
        , describe "filterMap"
            [ test "should work on json objects" <|
                \() ->
                    let
                        incIfNum key val =
                            case val of
                                Yajson.Number num ->
                                    Just ( "mapped " ++ key, Yajson.Number (num + 1) )

                                _ ->
                                    Nothing
                    in
                    Expect.equal
                        [ ( "mapped 1", Yajson.Number 1 ) ]
                        (Yajson.Object.filterMap incIfNum <|
                            Yajson.Object [ ( "1", Yajson.Number 0 ), ( "2", Yajson.String "s" ) ]
                        )
            ]
        , describe "filterMapKeys"
            [ test "should work on object keys" <|
                \() ->
                    let
                        mapKey key =
                            key
                                |> String.toInt
                                |> Maybe.map (\_ -> "int:" ++ key)
                    in
                    Expect.equal
                        [ ( "int:1", Yajson.Number 1 ) ]
                        (Yajson.Object.filterMapKeys mapKey <|
                            Yajson.Object.ofInt [ ( "a", 0 ), ( "1", 1 ) ]
                        )
            ]
        , describe "filterMapValues" <|
            [ test "should work on object values" <|
                \() ->
                    let
                        mapValue val =
                            case val of
                                Yajson.String s ->
                                    Just ("str:" ++ s)

                                _ ->
                                    Nothing
                    in
                    Expect.equal
                        [ ( "1", "str:value" ) ]
                        (Yajson.Object.filterMapValues mapValue <|
                            Yajson.Object [ ( "0", Yajson.Number 0 ), ( "1", Yajson.String "value" ) ]
                        )
            ]
        ]
