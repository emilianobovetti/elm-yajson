module Yajson.Stringify exposing (compact, pretty)

{-| Module for converting `Yajson.Json` to string.
May be useful for debugging purpose.

@docs compact, pretty

-}

import Yajson exposing (Json(..))


type Indent
    = None
    | Level Int


unescape : Char -> String -> String
unescape chr acc =
    case chr of
        '"' ->
            "\\\"" ++ acc

        '\\' ->
            "\\\\" ++ acc

        '\n' ->
            "\\n" ++ acc

        '\t' ->
            "\\t" ++ acc

        '\u{0008}' ->
            "\\b" ++ acc

        '\u{000C}' ->
            "\\f" ++ acc

        '\u{000D}' ->
            "\\r" ++ acc

        _ ->
            String.fromChar chr ++ acc


stringToSource : String -> String
stringToSource str =
    "\"" ++ String.foldr unescape "" str ++ "\""


beginObject : Indent -> String
beginObject ind =
    case ind of
        None ->
            "{"

        Level _ ->
            "{ "


beginArray : Indent -> String
beginArray ind =
    case ind of
        None ->
            "["

        Level _ ->
            "[ "


nameSeparator : Indent -> String
nameSeparator ind =
    case ind of
        None ->
            ":"

        Level _ ->
            ": "


valueSeparator : Indent -> String
valueSeparator ind =
    case ind of
        None ->
            ","

        Level _ ->
            indent ind ", "


nextLevel : Indent -> Json -> String
nextLevel ind json =
    case ind of
        Level lvl ->
            toString (Level (lvl + 1)) json

        None ->
            toString None json


append : String -> String -> String
append suffix str =
    str ++ suffix


foldObject : Indent -> ( String, Json ) -> String -> String
foldObject ind ( name, val ) acc =
    case acc of
        "" ->
            beginObject ind
                |> append (stringToSource name)
                |> append (nameSeparator ind)
                |> append (nextLevel ind val)

        _ ->
            acc
                |> append (valueSeparator ind)
                |> append (stringToSource name)
                |> append (nameSeparator ind)
                |> append (nextLevel ind val)


foldArray : Indent -> Json -> String -> String
foldArray ind val acc =
    case acc of
        "" ->
            beginArray ind
                |> append (nextLevel ind val)

        _ ->
            acc
                |> append (valueSeparator ind)
                |> append (nextLevel ind val)


indent : Indent -> String -> String
indent ind str =
    case ind of
        None ->
            str

        Level 0 ->
            "\n" ++ str

        Level lvl ->
            "\n" ++ String.repeat lvl "    " ++ str


toString : Indent -> Json -> String
toString ind json =
    case json of
        Object [] ->
            indent ind "{}"

        Object assoc ->
            List.foldl (foldObject ind) "" assoc
                |> append (indent ind "}")
                |> indent ind

        Array [] ->
            indent ind "[]"

        Array list ->
            List.foldl (foldArray ind) "" list
                |> append (indent ind "]")
                |> indent ind

        String str ->
            stringToSource str

        Number num ->
            String.fromFloat num

        Bool False ->
            "false"

        Bool True ->
            "true"

        Null ->
            "null"


{-| Json to string, no newlines, no spaces.

    str : String
    str =
        compact <| Array
            [ Object [ "name" => String "Hugo" ]
            , Object [ "name" => String "Manuel" ]
            , Object [ "name" => String "Eva" ]
            ]

    str == """[{"name":"Hugo"},{"name":"Manuel"},{"name":"Eva"}]"""

-}
compact : Json -> String
compact =
    toString None


{-| Json to string, with an indentation style close to elm-format's.

    str : String
    str =
        """[{"name":"Hugo"},{"name":"Manuel"},{"name":"Eva"}]"""
            |> fromString
            |> Result.withDefault Null
            |> pretty

    str ==
        """[
            { "name": "Hugo"
            }
        ,
            { "name": "Manuel"
            }
        ,
            { "name": "Eva"
            }
        ]"""

-}
pretty : Json -> String
pretty =
    toString (Level 0) >> String.trim
