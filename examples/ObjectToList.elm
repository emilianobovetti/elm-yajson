module ObjectToList exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (cols, rows)
import Html.Events exposing (onInput)
import Yajson
import Yajson.Array
import Yajson.Object
import Yajson.Stringify


type Msg
    = NewInput String


type alias Model =
    { json : Yajson.Json }


exampleJson : Yajson.Json
exampleJson =
    """
    [
        { "LBYCHxldU_Fdj0gGMwG":
            { "title": "Reservoir Dogs"
            , "year": 1992
            }
        }
    ,
        { "LBYFmCb7NEheQPcjYaE":
            { "title": "Pulp Fiction"
            , "year": 1994
            }
        }
    ,
        { "LBYFmXWnEFeqmlYkFa":
            { "title": "From Dusk Till Dawn"
            , "year": 1996
            }
        }
    ]
    """
        |> Yajson.fromString
        |> Result.withDefault Yajson.Null


init : ( Model, Cmd Msg )
init =
    ( { json = exampleJson }, Cmd.none )


update : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
update msg ( model, cmd ) =
    case msg of
        NewInput str ->
            let
                newJson : Yajson.Json
                newJson =
                    Yajson.fromString str |> Result.withDefault Yajson.Null

                newModel : Model
                newModel =
                    { model | json = newJson }
            in
            ( newModel, Cmd.none )


movieToString : Yajson.Json -> String
movieToString json =
    json
        |> Yajson.Object.map (\k v -> k ++ ": " ++ Yajson.Stringify.compact v)
        |> String.join ", "


viewMovies : Yajson.Json -> String
viewMovies json =
    json
        |> Yajson.Array.map Yajson.Object.values
        |> List.filterMap List.head
        |> List.map movieToString
        |> String.join "\n"


view : ( Model, Cmd Msg ) -> Html Msg
view ( { json }, cmd ) =
    Html.div []
        [ Html.h1 [] [ Html.text "Movie list" ]
        , Html.form []
            [ Html.textarea [ onInput NewInput, rows 24, cols 110 ]
                [ Html.text (Yajson.Stringify.pretty json) ]
            ]
        , Html.h2 [] [ Html.text "movies:" ]
        , Html.pre [] [ Html.text (viewMovies json) ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () ( Model, Cmd Msg ) Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
