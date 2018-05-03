# Yajson [![Build Status](https://travis-ci.org/emilianobovetti/elm-yajson.svg?branch=master)](https://travis-ci.org/emilianobovetti/elm-yajson)

I tried to port some of the [Yojson](https://github.com/mjambon/yojson) functionalities - in particular the [Yojson.Basic.Util](https://mjambon.github.io/mjambon2016/yojson-doc/Yojson.Basic.Util.html) module - in Elm.

This library provides dynamic json access and manipulation without the need to write [decoders](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Json-Decode#Decoder).

## Why

Elm has a great json library and ecosystem, but in certain situations, in an elm-repl session for example, I'd like to have a more flexible interface.
This library, anyway, was written to learn and explore Elm and the use case is more scholastic than practical. It's not meant to replace any of the standard library.

## Examples

Say we have a json like the following:

```elm
rawJson : String
rawJson =
    """
    { "squadName": "Super hero squad"
    , "homeTown": "Metro City"
    , "formed": 2016
    , "secretBase": "Super tower"
    , "active": true
    , "members":
        [
            { "name": "Molecule Man"
            , "age": 29
            , "secretIdentity": "Dan Jukes"
            , "powers":
                [ "Radiation resistance"
                , "Turning tiny"
                , "Radiation blast"
                ]
            }
        ,
            { "name": "Madame Uppercut"
            , "age": 39
            , "secretIdentity": "Jane Wilson"
            , "powers":
                [ "Million tonne punch"
                , "Damage resistance"
                , "Superhuman reflexes"
                ]
            }
        ,
            { "name": "Eternal Flame"
            , "age": 1000000
            , "secretIdentity": "Unknown"
            , "powers":
                [ "Immortality"
                , "Heat Immunity"
                , "Inferno"
                , "Teleportation"
                , "Interdimensional travel"
                ]
            }
        ]
    }
    """
```

if we want a list of powers, with standard library the code may look like this

```elm
powersDecoder : Json.Decode.Decoder (List (List String))
powersDecoder =
    Json.Decode.string
        |> Json.Decode.list
        |> Json.Decode.field "powers"
        |> Json.Decode.list
        |> Json.Decode.field "members"


powers : List String
powers =
    rawJson
        |> Json.Decode.decodeString powersDecoder
        |> Result.withDefault []
        |> List.concat
```

while with Yajson we could write

```elm
powers_ : List String
powers_ =
    rawJson
        |> Yajson.fromString
        |> Result.withDefault Yajson.Null
        |> Yajson.member "members"
        |> Maybe.map Yajson.toList
        |> Maybe.withDefault []
        |> Yajson.filterMember "powers"
        |> Yajson.flatten
        |> Yajson.filterString
```

For more usage examples look [here](https://github.com/emilianobovetti/elm-yajson/tree/master/examples).
