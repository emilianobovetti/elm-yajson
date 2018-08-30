module ExtractTitles exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (cols, rows)
import Html.Events exposing (onInput)
import Yajson
import Yajson.Stringify


type Msg
    = NewInput String


type alias Model =
    { json : Yajson.Json }


exampleJson : Yajson.Json
exampleJson =
    """
    { "id": "398eb027"
    , "name": "John Doe"
    , "pages":
        [
            { "id": 1
            , "title": "The Art of Flipping Coins"
            , "url": "http://example.com/398eb027/1"
            }
        ,
            { "id": 2
            , "deleted": true
            }
        ,
            { "id": 3
            , "title": "Artichoke Salad"
            , "url": "http://example.com/398eb027/3"
            }
        ,
            { "id": 4
            , "title": "Flying Bananas"
            , "url": "http://example.com/398eb027/4"
            }
        ]
    }
    """
        |> Yajson.fromString
        |> Result.withDefault Yajson.Null


init : Model
init =
    { json = exampleJson }


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewInput str ->
            let
                newJson : Yajson.Json
                newJson =
                    Yajson.fromString str |> Result.withDefault Yajson.Null
            in
            { model | json = newJson }


stringifyList : List String -> String -> String
stringifyList lst acc =
    case lst of
        [] ->
            acc

        [ last ] ->
            acc ++ last

        hd :: tl ->
            stringifyList tl (acc ++ hd ++ ", ")


showList : List String -> String
showList lst =
    "[ " ++ stringifyList lst "" ++ " ]"


viewTitles : Yajson.Json -> String
viewTitles json =
    [ json ]
        |> Yajson.filterMember "pages"
        |> Yajson.flatten
        |> Yajson.filterMember "title"
        |> Yajson.filterString
        |> showList


view : Model -> Html Msg
view { json } =
    Html.div []
        [ Html.h1 [] [ Html.text "Extract titles" ]
        , Html.form []
            [ Html.textarea [ onInput NewInput, rows 24, cols 110 ]
                [ Html.text (Yajson.Stringify.pretty json) ]
            ]
        , Html.h2 [] [ Html.text "titles:" ]
        , Html.pre [] [ Html.text (viewTitles json) ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
