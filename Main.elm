module Main exposing (..)

import Html exposing (Html, text, div, input)
import Html.Attributes exposing (type_)
import Html.Events exposing (onInput)
import Random
import Array
import BinarySearch


type alias Model =
    { data : List Int, key : Maybe Int }


type Msg
    = OrderData
    | ReceiveData (List Int)
    | NewNumber String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OrderData ->
            ( model, cmdRandom )

        ReceiveData data ->
            ( { model | data = List.sort data }, Cmd.none )

        NewNumber text ->
            { model | key = Result.toMaybe (String.toInt text) } ! []


cmdRandom : Cmd Msg
cmdRandom =
    Random.generate ReceiveData (Random.list 100 (Random.int -100 100))


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "number", onInput NewNumber ] []
        , div [] [ text ("data: " ++ toString model.data) ]
        , div [] [ text ("length: " ++ toString (List.length model.data)) ]
        , result model
        ]


result : Model -> Html msg
result model =
    case model.key of
        Nothing ->
            text "Please enter number"

        Just key ->
            div []
                [ text "Found at index: "
                , model.data
                    |> Array.fromList
                    |> BinarySearch.run key
                    |> toString
                    |> text
                ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( { data = [], key = Nothing }, cmdRandom )
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
