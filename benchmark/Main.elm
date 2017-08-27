module Main exposing (..)

--import Benchmark exposing (Benchmark, benchmark3)
--import Benchmark.Runner exposing (BenchmarkProgram, program)
import Benchmark.LowLevel as B
import Html exposing (Html, div, text)
import Medium exposing (points)
import Simplify as S exposing (PixelTolerance, Quality)
import Task
import Time exposing (Time)


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias Model =
  { dataTotal : Int
  , bench : Maybe Time
  , duration : Maybe Time
  }

type Msg
  = Bench (Result B.Error Time)
  | Measure (List (Float, Float), Time)


init : (Model, Cmd Msg)
init =
  ( Model (List.length points) Nothing Nothing
  , Cmd.batch
    [ B.operation3 S.simplify S.OnePixel S.Low points
        |> B.sample 1
        |> Task.attempt Bench
    , simplifyCmd S.OnePixel S.Low points
    ]
  )

simplifyCmd : PixelTolerance -> Quality -> List (Float, Float) -> Cmd Msg
simplifyCmd pxlt q points =
  Task.perform Measure <|
    ( Time.now
      |> Task.andThen (\t1 ->
        Task.map2 (\spoints t2 -> (spoints, t2 - t1))
          (S.simplify pxlt q points |> Task.succeed)
          Time.now
      )
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Bench (Ok t) ->
      ( {model | bench = Just t}
      , Cmd.none
      )
    Bench (Err _) ->
      ( model
      , Cmd.none
      )
    Measure (spoints, t) ->
      ( {model | duration = Just t}
      , Cmd.none
      )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
  div []
    [ div []
      ( model.bench
          |> Maybe.map (\d ->
            [ text <| "Simplify: input = " ++ (toString <| model.dataTotal) ++ (" | bench duration = ") ++ (toString d) ++ " ms"
            ]
          )
          |> Maybe.withDefault []
      )
    , div []
      ( model.duration
          |> Maybe.map (\d ->
            [ text <| "Simplify: input = " ++ (toString <| model.dataTotal) ++ (" | task duration = ") ++ (toString d) ++ " ms"
            ]
          )
          |> Maybe.withDefault []
      )
    ]

{-}
main : BenchmarkProgram
main =
    program <|
        Benchmark.describe "elm-simplify: radial + ramer-douglas-peucker"
            [ benchmark3 "simplify"
                S.simplify
                S.OnePixel
                S.Low
                points
            ]
-}
