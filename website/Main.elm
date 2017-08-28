module Main exposing (..)

{-| Demo of elm-simplify.
-}
import Collage as C exposing (Form, defaultLine)
import Color
import Dict exposing (Dict)
import Element as E
import Json.Decode exposing (Decoder, field, Value)
import Json.Encode
import Html exposing
  (Html
  , a
  , br
  , canvas, code
  , dd, div, dl, dt
  , em
  , h1
  , iframe, input
  , label
  , p
  , span, strong
  , text
  )
import Html.Attributes as HAttr exposing
  ( class
  , height, href
  , id
  , src, style
  , type_
  , value
  , width
  )
import Html.Events as HEvt exposing (onCheck, onInput)
import Simplify as S exposing (PixelTolerance, Quality)
import Task
import Time exposing (Time)
import Transform as T exposing (Transform)


main : Program Flags Model Msg
main =
  Html.programWithFlags
   { init = init
   , update = update
   , subscriptions = subscriptions
   , view = view
   }


{-| Data passed from JS when Elm application is initialized.
-}
type alias Flags =
  { data : Value
  }

{-| Application model aka state
-}
type alias Model =
  { csize : { width : Int, height : Int}
  , data : Dict Int (Float, Float)
  , dataTotal : Int
  , duration : Maybe Time
  , pixelTolerance : PixelTolerance
  , quality : Quality
  , simplified : Dict Int (Float, Float)
  , simplifiedTotal : Maybe Int
  , sliderTolerance : PixelTolerance
  }

type Msg
  = Measure (Dict Int (Float, Float), Time)
  | QualityHigh Bool
  | Slider String
  | Tolerance String


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
  let
    initData =
      Json.Decode.decodeValue decoder flags.data
          |> Result.withDefault []
          |> List.indexedMap (,)
          |> Dict.fromList
    initTolerance = S.Pixels 0.8
    initQuality = S.Low
  in
    ( { csize = { width = 720, height = 400 }
      , data = initData
      , dataTotal = Dict.size initData
      , duration = Nothing
      , pixelTolerance = initTolerance
      , quality = initQuality
      , simplified = Dict.empty
      , simplifiedTotal = Nothing
      , sliderTolerance = initTolerance
      }
    , simplifyCmd initTolerance initQuality initData
    )


{-| Helper to convert PixelTolerance to Float
-}
pxlf : PixelTolerance -> Float
pxlf pxlt =
  case pxlt of
    S.OnePixel -> 1.0
    S.Pixels p -> p


{-| Run simplification with timing
-}
simplifyCmd : PixelTolerance -> Quality -> Dict Int (Float, Float) -> Cmd Msg
simplifyCmd pxlt q points =
  Task.perform Measure <|
    ( Time.now
      |> Task.andThen (\t1 ->
        Task.map2 (\spoints t2 -> (spoints, t2 - t1))
          (S.simplify pxlt q points |> Task.succeed)
          Time.now
      )
    )


{-|-}
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


{-|-}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Measure (spoints, t) ->
      ( { model
          | duration = Just <| t / 1.5 -- approx. diff between elm-benchmark and task based measurements
          , simplified = spoints
          , simplifiedTotal = Just <| Dict.size spoints
        }
      , Cmd.none
      )
    QualityHigh flag ->
      let
        q = if flag == True then S.High else S.Low
      in
        ( {model | quality = q}
        , simplifyCmd model.pixelTolerance q model.data
        )
    Slider s ->
      String.toFloat s
        |> Result.map (\f ->
          ( {model | sliderTolerance = S.Pixels f}
          , Cmd.none
          )
        )
        |> Result.withDefault (model, Cmd.none)
    Tolerance s ->
      let
        (m, _) =
          String.toFloat s
            |> Result.map (\f ->
              ( {model | pixelTolerance = S.Pixels f}
              , Cmd.none
              )
            )
            |> Result.withDefault (model, Cmd.none)
      in
        ( m
        , simplifyCmd m.pixelTolerance m.quality m.data
        )


{-|-}
createCanvas : Model -> Html Msg
createCanvas model =
  let
    pathStyle =
      { defaultLine
        | color = Color.rgb 255 85 51
        , width = 2
        , cap = C.Round
      }
  in
    [ model.simplified
        |> Dict.values
        |> List.map (toCanvasPoint model)
        |> C.path
        |> C.traced pathStyle
    ]
      |> C.groupTransform (T.translation -200 100)
      |> (\f -> [f])
      |> C.collage model.csize.width model.csize.height
      |> E.toHtml


{-| Shift coordinates for Elm's canvas, where (0,0) is center not top/left.
-}
toCanvasPoint : Model -> (Float, Float) -> (Float, Float)
toCanvasPoint model (x, y) =
  ( x - (toFloat (model.csize.width // 2))
  , (toFloat (model.csize.height // 2)) - y
  )


{-|-}
view : Model -> Html Msg
view model =
  div
    [ class "container" ]
    [ h1
      []
      [ text "elm-simplify"
      ]

    , p [ class "social-buttons" ]
      [ iframe
        [ src "http://ghbtns.com/github-btn.html?user=s6o&amp;repo=elm-simplify&amp;type=watch&amp;count=true"
        , HAttr.attribute "allowtransparency" "true"
        , HAttr.attribute "frameborder" "0"
        , HAttr.attribute "scrolling" "0"
        , HAttr.attribute "width" "95px"
        , HAttr.attribute "height" "20px"
        ]
        []
      ]

    , p
      []
      [ text "elm-simplify ("
      , strong [] [ text "License" ]
      , text ": "
      , em [] [ text "BSD"]
      , text ", "
      , strong [] [ text "GitHub" ]
      , text ": "
      , a
        [ href "http://github.com/s6o/elm-simplify"
        ]
        [ text "s6o / elm-simplify"
        ]
      , text ", "
      , strong [] [ text "package.elm-lang.org" ]
      , text ": "
      , em [] [ text "s6o/elm-simplify" ]
      , text <| ") is a port of the 2D variant of the tiny high-performance"
        ++ " JavaScript polyline simplification library "
      , a
          [ href "http://mourner.github.io/simplify-js/" ]
          [ text " Simplify.js" ]
      , text ". It uses a combination of "
      , a
          [ href "http://en.wikipedia.org/wiki/Ramer-Douglas-Peucker_algorithm" ]
          [ text "Ramer-Douglas-Peucker" ]
      , text " and Radial Distance algorithms."
      ]

    , p []
      [ text <|
        "Polyline simplification dramatically reduces the number of points in a"
      ++ " polyline while retaining its shape, giving a huge performance boost when"
      ++ " processing it and also reducing visual noise. For example, it's essential"
      ++ " when rendering a 70k-points line chart or a map route in the browser using Canvas or SVG."
      ]

    , p []
      [ text <|
        "NOTE: due to Elm's runtime, time measurements are approximations."
      ]

    , div
      [ class "canvas-container cf" ]
      [ p
        [ class "tolerance-container" ]
        [ label []
          [ input
            [ id "tolerance"
            , type_ "range"
            , HAttr.min "0.10"
            , HAttr.max "5.00"
            , HAttr.step "0.01"
            , value <| toString <| pxlf model.sliderTolerance
            , style [("width", "100%")]
            , HEvt.on "change" (Json.Decode.map Tolerance HEvt.targetValue)
            , onInput Slider
            ]
            []
          , text <| toString <| pxlf model.sliderTolerance
          ]
        ]

      , p [ class "stats" ]
        [ em [ id "point-num-before" ] [ text <| toString <| model.dataTotal ]
        , text " points, simplified with tolerance: "
        , em [ id "tolerance-val" ] [ text <| toString <| pxlf model.pixelTolerance ]
        , text " px"
        , br [] []
        , text "After simplification: "
        , em [ id "point-num-after" ]
          ( model.simplifiedTotal
              |> Maybe.map (\stotal -> [ text <| toString <| stotal ])
              |> Maybe.withDefault []
          )
        , text " points (~"
        , em [ id "point-num-times" ]
          ( model.simplifiedTotal
              |> Maybe.map (\stotal -> [ text <| toString <| round <| (toFloat model.dataTotal) / (toFloat stotal) ])
              |> Maybe.withDefault []
          )
        , text " times less)"
        , br [] []
        , text "Performed in ~"
        , em [ id "duration" ]
          ( model.duration
              |> Maybe.map (\t -> [ text <| toString t ])
              |> Maybe.withDefault []
          )
        , text " ms"
        ]

      , p [ class "quality" ]
        [ label
          []
          [ input
            [ id "quality"
            , type_ "checkbox"
            , onCheck QualityHigh
            ]
            []
          , text " highest quality"
          ]
        ]

      , p [ class "attribution" ]
        [ text <|
            "The test data for the example is actually a ~10700 mile car route"
        ++ " from Lisboa, Portugal to Singapore on a world scale, generated by the "
        , a [ href "http://developers.cloudmade.com/wiki/navengine/Documentation" ]
          [ text "CloudMade Navigation service"
          ]
        , text " based on "
        , a [ href "http://openstreetmap.org" ]
          [ text "OpenStreetMap"
          ]
        , text " data."
        ]

      , div
        [ id "canvas"
        , style
          [ ("width", (toString model.csize.width) ++ "px")
          , ("height", (toString model.csize.height) ++"px")
          , ("padding", "0")
          , ("margin", "0")
          ]
        ]
        [ createCanvas model
        ]
      ]

    , dl []
      [ dt []
        [ code []
          [ text "simplifyDefault : Dict Int (Float, Float) -> Dict Int (Float, Float)"
          ]
        ]
      , dd []
        [ p []
          [ text <| "Convenience function. `PixelTolerance` is set to `OnePixel` "
            ++ "and `Quality` is set to `Low`. Returns a list of simplified points."
          ]
        ]

      , dt []
        [ code []
          [ text "simplify : PixelTolerance -> Qaulity -> Dict Int (Float, Float) -> Dict Int (Float, Float)"
          ]
        ]
      , dd []
        [ p [] [ text "Returns a list of simplified points." ]
        ]

      , dt []
        [ code []
          [ text
            """
            type PixelTolerance
              = OnePixel
              | Pixels Float
            """
          ]
        ]
      , dd []
        [ p []
          [ text "Affects the amount of simplification (in the same metric as the point coordinates)."
          ]
        ]

      , dt []
        [ code []
          [ text
            """
            type Quality
              = Low
              | High
            """
          ]
        ]
      , dd []
        [ p []
          [ text <| "`High` excludes distance-based pre-processing step which leads to"
            ++ " highest quality simplification, but runs slower."
          ]
        ]
      ]

      , dt []
        [ code []
          [ text "points : Dict Int (Float, Float)" ]
        ]
      , dd []
        [ p []
          [ text "A dictionary of tuples - pairs of x, y coordinates - of "
          , code [] [ text "(Float, Float)" ]
          ]
        , p []
          [ text "Note: the 3D version of Simplify.js is not yet supported."
          ]
        ]

    , p [ class "footer" ]
      [ span [ HAttr.property "innerHTML" (Json.Encode.string "&copy;") ] []
      , text " 2017, "
      , a [ href "https://github.com/s6o/elm-simplify" ]
        [ text "Oliver Sõro"
        ]
      , text ". Released under "
      , a [ href "https://raw.github.com/s6o/elm-simplify/master/LICENSE" ]
        [ text "BSD license"
        ]
      , text "."
      ]
    ]