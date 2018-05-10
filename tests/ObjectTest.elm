module ObjectTest exposing (..)

import Test exposing (Test, describe, test)
import Yajson exposing (Json(Object, String, Number, Bool, Null))
import Yajson.Infix exposing ((=>))
import Yajson.Object as Obj
import Expect


suite : Test
suite =
    describe "Yajson.Object"
        [ describe "ofString"
            [ test "should create an object with string values" <|
                \() ->
                    Expect.equal
                        (Object
                            [ "1" => String "1"
                            , "2" => String "2"
                            ]
                        )
                        (Obj.ofString
                            [ "1" => "1"
                            , "2" => "2"
                            ]
                        )
            ]
        , describe "ofFloat"
            [ test "should create an object with float values" <|
                \() ->
                    Expect.equal
                        (Object
                            [ "1" => Number 1
                            , "2" => Number 2
                            ]
                        )
                        (Obj.ofFloat
                            [ "1" => 1
                            , "2" => 2
                            ]
                        )
            ]
        , describe "ofInt"
            [ test "should create an object with int values" <|
                \() ->
                    Expect.equal
                        (Object
                            [ "1" => Number 1
                            , "2" => Number 2
                            ]
                        )
                        (Obj.ofInt
                            [ "1" => 1
                            , "2" => 2
                            ]
                        )
            ]
        , describe "ofBool"
            [ test "should create an object with int boolean" <|
                \() ->
                    Expect.equal
                        (Object
                            [ "1" => Bool True
                            , "2" => Bool False
                            ]
                        )
                        (Obj.ofBool
                            [ "1" => True
                            , "2" => False
                            ]
                        )
            ]
        , describe "keys"
            [ test "should return object keys" <|
                \() ->
                    Expect.equal
                        [ "1", "2", "3" ]
                        (Obj.keys <|
                            Obj.ofInt
                                [ "1" => 1
                                , "2" => 2
                                , "3" => 3
                                ]
                        )
            ]
        , describe "values"
            [ test "should return object values" <|
                \() ->
                    Expect.equal
                        [ Number 1, Number 2, Number 3 ]
                        (Obj.values <|
                            Obj.ofInt
                                [ "1" => 1
                                , "2" => 2
                                , "3" => 3
                                ]
                        )
            ]
        , describe "map"
            [ test "should map json object keys and values" <|
                \() ->
                    let
                        incKeyVal key val =
                            case ( String.toInt key, val ) of
                                ( Ok keyNum, Number valNum ) ->
                                    ( toString (keyNum + 1), Number (valNum + 1) )

                                _ ->
                                    ( key, val )
                    in
                        Expect.equal
                            ([ "1" => Number 1, "2" => Number 2 ])
                            (Obj.map incKeyVal <| Obj.ofInt [ "0" => 0, "1" => 1 ])
            ]
        , describe "mapKeys"
            [ test "should map json object keys" <|
                \() ->
                    let
                        incKey key =
                            case String.toInt key of
                                Ok keyNum ->
                                    toString (keyNum + 1)

                                _ ->
                                    key
                    in
                        Expect.equal
                            [ "1" => Number 1, "2" => Number 2 ]
                            (Obj.mapKeys incKey <| Obj.ofInt [ "0" => 1, "1" => 2 ])
            ]
        , describe "mapValues"
            [ test "should map json object values" <|
                \() ->
                    let
                        incVal val =
                            case val of
                                Number num ->
                                    Number (num + 1)

                                _ ->
                                    val
                    in
                        Expect.equal
                            [ "1" => Number 1, "2" => Number 2 ]
                            (Obj.mapValues incVal <| Obj.ofInt [ "1" => 0, "2" => 1 ])
            ]
        , describe "filterMap"
            [ test "should work on json objects" <|
                \() ->
                    let
                        incIfNum key val =
                            case val of
                                Number num ->
                                    Just ( "mapped " ++ key, Number (num + 1) )

                                _ ->
                                    Nothing
                    in
                        Expect.equal
                            [ "mapped 1" => Number 1 ]
                            (Obj.filterMap incIfNum <| Object [ "1" => Number 0, "2" => String "s" ])
            ]
        , describe "filterMapKeys"
            [ test "should work on object keys" <|
                \() ->
                    let
                        mapKey key =
                            case String.toInt key of
                                Ok _ ->
                                    Just ("int:" ++ key)

                                Err _ ->
                                    Nothing
                    in
                        Expect.equal
                            [ "int:1" => Number 1 ]
                            (Obj.filterMapKeys mapKey <| Obj.ofInt [ "a" => 0, "1" => 1 ])
            ]
        , describe "filterMapValues" <|
            [ test "should work on object values" <|
                \() ->
                    let
                        mapValue val =
                            case val of
                                String s ->
                                    Just ("str:" ++ s)

                                _ ->
                                    Nothing
                    in
                        Expect.equal
                            [ "1" => "str:value" ]
                            (Obj.filterMapValues mapValue <| Object [ "0" => Number 0, "1" => String "value" ])
            ]
        ]
