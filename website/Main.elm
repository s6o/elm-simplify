module Main exposing (..)

{-| Demo of elm-simplify.
-}
import Json.Decode exposing (Decoder, field, Value)
import Html exposing (Html, div, text)


main : Program Flags Model Msg
main =
  Html.programWithFlags
   { init = init
   , update = update
   , subscriptions = subscriptions
   , view = view
   }


type alias Flags =
  { data : Value
  }

type alias Model =
  { data : List (Float, Float)
  }

type Msg
  = NoOp


{-| JSON decoder for test data loded via `Flags`
-}
decoder : Decoder (List (Float, Float))
decoder =
  Json.Decode.list
    <| Json.Decode.map2
        (\x y -> (x, y))
        (field "x" Json.Decode.float)
        (field "y" Json.Decode.float)


{-|-}
init : Flags -> (Model, Cmd Msg)
init flags =
  ( Json.Decode.decodeValue decoder flags.data
      |> Result.withDefault []
      |> Model
  , Cmd.none
  )


{-|-}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  ( model
  , Cmd.none
  )


{-|-}
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


{-|-}
view : Model -> Html Msg
view model =
  div
    []
    [ text <| "Loaded test-point count: " ++ (List.length model.data |> toString)
    ]