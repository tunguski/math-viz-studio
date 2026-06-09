module Viz.ConvexHull exposing (viz)

{-| **Convex hull** — the smallest convex polygon containing a set of points: the shape a rubber band
would snap to around them. Computed here by Andrew's monotone-chain algorithm on a seeded random
scatter; the hull is outlined over the points.
-}

import Color
import Draw
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as A
import Value
import Viz exposing (Viz)


type alias Model =
    { count : Int
    , seed : Int
    , stroke : String
    }


viz : Viz
viz =
    { kind = "hull"
    , name = "Convex hull"
    , description = "The rubber-band polygon around a cloud of points."
    , about = "The convex hull of a set of points is the smallest convex region that contains them all — exactly the shape a rubber band stretched around a board of pins would take. It is the \"hello world\" of computational geometry.\n\nComputing it efficiently is a classic problem with elegant solutions (Graham scan, gift wrapping, the monotone chain used here, all running in n log n time). Hulls are a workhorse underneath collision detection, pattern recognition, path planning and statistics — wherever you need the outer boundary of scattered data."
    , starter = toSource default
    , movable = False
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { count = 40, seed = 7, stroke = "#fbbf24" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map3 Model
                    (Value.intField "count" fs)
                    (Value.intField "seed" fs)
                    (Value.strField "stroke" fs)
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"hull\"\n    , count = "
        ++ String.fromInt d.count
        ++ "\n    , seed = "
        ++ String.fromInt d.seed
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"



-- RENDER ------------------------------------------------------------------------------------------


prepare : Model -> (String -> Float -> Svg msg)
prepare d =
    let
        pts =
            points (clamp 3 400 d.count) (max 1 d.seed)

        hullPts =
            hull (List.sort pts)

        scale =
            270

        screen ( x, y ) =
            ( x * scale, -y * scale )

        hullStr =
            String.join " " (List.map (\p -> let ( sx, sy ) = screen p in Draw.r1 sx ++ "," ++ Draw.r1 sy) hullPts)
    in
    \mode phase ->
        let
            color =
                Color.solid (Color.resolve mode d.stroke) phase

            dot p =
                let
                    ( sx, sy ) =
                        screen p
                in
                Svg.circle [ A.cx (Draw.r1 sx), A.cy (Draw.r1 sy), A.r "2.6", A.fill "#8aa0b2" ] []
        in
        Draw.stage
            (Svg.polygon [ A.points hullStr, A.fill "none", A.stroke color, A.strokeWidth "2", A.strokeLinejoin "round", A.opacity "0.95" ] []
                :: List.map dot pts
            )


points : Int -> Int -> List ( Float, Float )
points n seed0 =
    let
        lcg s =
            modBy 2147483647 (48271 * s)

        val s =
            toFloat s / 2147483647 * 1.8 - 0.9

        step _ ( s, acc ) =
            let
                s1 =
                    lcg s

                s2 =
                    lcg s1
            in
            ( s2, ( val s1, val s2 ) :: acc )
    in
    Tuple.second (List.foldl step ( seed0, [] ) (List.range 1 n))


{-| Andrew's monotone chain on points already sorted by (x, y). -}
hull : List ( Float, Float ) -> List ( Float, Float )
hull sorted =
    let
        push p stack =
            case stack of
                b :: a :: rest ->
                    if cross a b p <= 0 then
                        push p (a :: rest)

                    else
                        p :: stack

                _ ->
                    p :: stack

        half ps =
            List.foldl push [] ps

        lower =
            half sorted

        upper =
            half (List.reverse sorted)
    in
    dropLast (List.reverse lower) ++ dropLast (List.reverse upper)


cross : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Float
cross ( ax, ay ) ( bx, by ) ( px, py ) =
    (bx - ax) * (py - ay) - (by - ay) * (px - ax)


dropLast : List a -> List a
dropLast xs =
    List.take (max 0 (List.length xs - 1)) xs



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a convex hull yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Points"
                    [ Form.slider "count" 3 400 1 (toFloat d.count) (\v -> toSource { d | count = round v })
                    , Form.slider "seed" 1 200 1 (toFloat d.seed) (\v -> toSource { d | seed = round v })
                    , Form.colorRow "hull" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Few" (toSource { d | count = 12, seed = 3 })
                        , Form.preset "Seed 7" (toSource { d | count = 40, seed = 7 })
                        , Form.preset "Seed 23" (toSource { d | count = 40, seed = 23 })
                        , Form.preset "Many" (toSource { d | count = 160, seed = 11 })
                        , Form.preset "Dense" (toSource { d | count = 350, seed = 42 })
                        ]
                    ]
                ]
