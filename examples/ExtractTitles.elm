module ExtractTitles exposing (main)

import Html.Attributes exposing (rows, cols)
import Html.Events exposing (onInput)
import Html exposing (Html)
import Yajson.Stringify
import Yajson


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


viewTitles : Yajson.Json -> List String
viewTitles json =
    [ json ]
        |> Yajson.filterMember "pages"
        |> Yajson.flatten
        |> Yajson.filterMember "title"
        |> Yajson.filterString


view : Model -> Html Msg
view { json } =
    Html.div []
        [ Html.h1 [] [ Html.text "Extract titles" ]
        , Html.form []
            [ Html.textarea [ onInput NewInput, rows 24, cols 110 ]
                [ Html.text (Yajson.Stringify.pretty json) ]
            ]
        , Html.h2 [] [ Html.text "titles:" ]
        , Html.pre [] [ Html.text (viewTitles json |> toString) ]
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
