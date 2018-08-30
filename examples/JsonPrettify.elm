module JsonPrettify exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (cols, rows)
import Html.Events exposing (onInput)
import Json.Decode as Decode
import Yajson
import Yajson.Stringify


type Msg
    = NewInput String


type alias Model =
    { json : Result Decode.Error Yajson.Json }


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


update : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
update msg ( model, cmd ) =
    case msg of
        NewInput str ->
            ( { model | json = Yajson.fromString str }, Cmd.none )


viewJson : Result Decode.Error Yajson.Json -> String
viewJson res =
    case res of
        Ok json ->
            Yajson.Stringify.pretty json

        Err err ->
            Decode.errorToString err


view : ( Model, Cmd Msg ) -> Html Msg
view ( { json }, cmd ) =
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


main : Program () ( Model, Cmd Msg ) Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
