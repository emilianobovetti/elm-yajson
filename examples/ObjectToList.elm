module ObjectToList exposing (main)

import Html.Attributes exposing (rows, cols)
import Html.Events exposing (onInput)
import Html exposing (Html)
import Yajson.Stringify
import Yajson.Object
import Yajson.Array
import Yajson


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
    { json = exampleJson } ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewInput str ->
            { model
                | json =
                    Yajson.fromString str
                        |> Result.withDefault Yajson.Null
            }
                ! []


movieToString : Yajson.Json -> String
movieToString json =
    json
        |> Yajson.Object.map (\k v -> k ++ ": " ++ Yajson.Stringify.compact v)
        |> List.foldr (\v acc -> v ++ ", " ++ acc) ""
        |> String.dropRight 2


viewMovies : Yajson.Json -> String
viewMovies json =
    json
        |> Yajson.Array.map Yajson.Object.values
        |> List.filterMap List.head
        |> List.map movieToString
        |> List.foldr (\v acc -> v ++ "\n" ++ acc) ""


view : Model -> Html Msg
view { json } =
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


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
