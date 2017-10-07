module Simplify exposing
  ( PixelTolerance(..)
  , Quality(..)
  , simplify
  , simplifyDefault
  )

{-| Port of Simplify.js http://mourner.github.io/simplify-js/

Interactive demo at https://s6o.github.io/elm-simplify/

@docs PixelTolerance, Quality, simplify, simplifyDefault

-}

import Dict exposing (Dict)


{-| Simplification pixel tolerance.
-}
type PixelTolerance
  = OnePixel
  | Pixels Float

{-| Simplification quality.
    Two options:
      * Low = Radial Distance + Ramer-Douglas-Peucker
      * High = only Ramer-Douglas-Peucker
-}
type Quality
  = Low
  | High


{-| Simplify with Radial Distance and/or Ramer-Douglas-Peucker
-}
simplify : PixelTolerance -> Quality -> Dict Int (Float, Float) -> Dict Int (Float, Float)
simplify tolerance quality points =
  if Dict.size points <= 2 then
    points
  else
    let
      sqTolerance =
        case tolerance of
          OnePixel -> 1
          Pixels p -> p * p
      newPoints =
        case quality of
          Low -> simplifyRadialDistance points sqTolerance
          High -> points
    in
      simplifyDouglasPeucker newPoints sqTolerance

{-| Convenience function for the typical use case equal to:

    simplify OnePixel Low
-}
simplifyDefault : Dict Int (Float, Float) -> Dict Int (Float, Float)
simplifyDefault points =
  simplify OnePixel Low points

{-| Basic distance-based simplification
-}
simplifyRadialDistance : Dict Int (Float, Float) -> Float -> Dict Int (Float, Float)
simplifyRadialDistance points sqTolerance =
  let
    checkSqDist pointList sqt (index, accum) =
      case (pointList, List.head accum) of
        ([], _) ->
          (index, accum)
        (x :: [], _) ->
          (index + 1, (index, x) :: accum)
        (x :: rest, Nothing) ->
          checkSqDist rest sqt (index + 1, (index, x) :: accum)
        (x :: rest, Just (_, p)) ->
          let
            sqDist = squareDistance x p
          in
            if sqDist > sqt then
              checkSqDist rest sqt (index + 1, (index, x) :: accum)
            else
              checkSqDist rest sqt (index, accum)
  in
    checkSqDist (Dict.values points) sqTolerance (0, [])
      |> (\(_, accum) -> Dict.fromList accum)

{-| Square distance between 2 points.
-}
squareDistance : (Float, Float) -> (Float, Float) -> Float
squareDistance (p1_x, p1_y) (p2_x, p2_y) =
  let
    dx = p1_x - p2_x
    dy = p1_y - p2_y
  in
    (dx * dx) + (dy * dy)

{-| Simplification using Ramer-Douglas-Peucker algorithm
-}
simplifyDouglasPeucker : Dict Int (Float, Float) -> Float -> Dict Int (Float, Float)
simplifyDouglasPeucker points sqTolerance =
  let
    firstIndex = 0
    lastIndex = (Dict.size points) - 1
    firstPoint = Dict.get firstIndex points
    lastPoint = Dict.get lastIndex points
  in
    simplifyDPStep points firstIndex lastIndex sqTolerance ([], [])
      |> (\(_, accum) ->
        case (firstPoint, lastPoint) of
          (Just fp, Just lp) ->
              (firstIndex, fp) :: (lastIndex, lp) :: accum |> Dict.fromList
          _ ->
            Debug.crash "Should not be here: something went wrong in the public interface"
      )

{-|-}
simplifyDPStep : Dict Int (Float, Float) -> Int -> Int -> Float -> (List (Int, Int), List (Int, (Float, Float))) -> (List (Int, Int), List (Int, (Float, Float)))
simplifyDPStep points firstIndex lastIndex sqTolerance (accumRanges, accumPoints) =
  let
    (maxSqDist, maxIndex, maxPoint) =
      findMaxSquareSegmentDistance points firstIndex lastIndex sqTolerance
  in
    case maxSqDist > sqTolerance of
      False ->
        case accumRanges of
          [] ->
            (accumRanges, accumPoints)
          (f, t) :: rest ->
            simplifyDPStep points f t sqTolerance (rest, accumPoints)
      True ->
        let
          nextPoints = (maxIndex, maxPoint) :: accumPoints
          nextRanges =
            [ (maxIndex - firstIndex > 1, (firstIndex, maxIndex))
            , (lastIndex - maxIndex > 1, (maxIndex, lastIndex))
            ]
              |> List.foldl (\(cond, range) accum ->
                if cond then range :: accum else accum
              ) accumRanges
        in
          case nextRanges of
            [] ->
              (nextRanges, nextPoints)
            (f, t) :: rest ->
              simplifyDPStep points f t sqTolerance (rest, nextPoints)

{-|-}
findMaxSquareSegmentDistance : Dict Int (Float, Float) -> Int -> Int -> Float -> (Float, Int, (Float, Float))
findMaxSquareSegmentDistance points firstIndex lastIndex sqTolerance =
  let
    firstPoint = Dict.get firstIndex points
    lastPoint = Dict.get lastIndex points
    initialIndex = firstIndex + 1
    indexes = List.range initialIndex (lastIndex - 1)
  in
    Maybe.map2 (\fp lp ->
      indexes
        |> List.foldl (\i (m, mi, mp) ->
          points
            |> Dict.get i
            |> Maybe.map (\p ->
              let
                sqDist = squareSegmentDistance p fp lp
              in
                if sqDist > m then
                  (sqDist, i, p)
                else
                  (m, mi, mp)
                )
            |> Maybe.withDefault (m, mi, mp)
        ) (sqTolerance, initialIndex, (0, 0))
    ) firstPoint lastPoint
    |> Maybe.withDefault (sqTolerance, initialIndex, (0, 0))

{-| Square distance from a point to a segment
-}
squareSegmentDistance : (Float, Float) -> (Float, Float) -> (Float, Float) -> Float
squareSegmentDistance (p_x, p_y) (p1_x, p1_y) (p2_x, p2_y) =
  let
    xy = (p1_x, p1_y)
    dx = p2_x - p1_x
    dy = p2_y - p1_y
    newXY =
      if dx /= 0 || dy /= 0 then
        let
          (x, y) = xy
        in
          let
            t = ((((p_x - x) * dx) + ((p_y - y) * dy))) / ((dx * dx) + (dy * dy))
          in
            if t > 1 then
              ( p2_x
              , p2_y
              )
            else
              if t > 0 then
                ( x + (dx * t)
                , y + (dy * t)
                )
              else
                xy
      else
        xy
  in
    let
      (x, y) = newXY
    in
      let
        dx = p_x - x
        dy = p_y - y
      in
        (dx * dx) + (dy * dy)
