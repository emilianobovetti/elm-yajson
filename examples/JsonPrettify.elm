module JsonPrettify exposing (main)

import Html.Attributes exposing (rows, cols)
import Html.Events exposing (onInput)
import Html exposing (Html)
import Json.Decode as Decode
import Yajson.Stringify
import Yajson


type Msg
    = NewInput String


type alias Model =
    { json : Result String Yajson.Json }


rawJson : String
rawJson =
    """
    {"widget": {
      "debug": "on",
      "window": {
        "title": "Sample Konfabulator Widget", "name": "main_window", "width": 500, "height": 500
      },
      "image": {
        "src": "Images/Sun.png", "name": "sun1", "hOffset": 250, "vOffset": 250, "alignment": "center"
      },
      "text": {
        "data": "Click Here", "size": 36, "style": "bold", "name": "text1", "hOffset": 250,
        "vOffset": 100, "alignment": "center", "onMouseUp": "sun1.opacity = (sun1.opacity / 100) * 90;"
      }
    }}
    """


init : ( Model, Cmd Msg )
init =
    ( { json = Yajson.fromString rawJson }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewInput str ->
            ( { model | json = Yajson.fromString str }, Cmd.none )


viewJson : Result String Yajson.Json -> String
viewJson res =
    case res of
        Ok json ->
            Yajson.Stringify.pretty json

        Err e ->
            Decode.errorToString e


view : Model -> Html Msg
view { json } =
    Html.div []
        [ Html.h1 [] [ Html.text "Json prettify" ]
        , Html.form []
            [ Html.textarea [ onInput NewInput, rows 15, cols 110 ]
                [ Html.text rawJson ]
            ]
        , Html.pre [] [ Html.text (viewJson json) ]
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
