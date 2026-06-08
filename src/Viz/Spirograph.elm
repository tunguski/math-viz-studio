module Viz.Spirograph exposing (viz)

{-| **Spirograph** — the hypotrochoid traced by a pen at offset `pen` inside a small circle of radius
`smallR` rolling inside a big circle of radius `bigR`. The toy made these famous; the maths is a pair
of integers whose ratio sets the number of loops.
-}

import Color
import Draw
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Value
import Viz exposing (Viz)


type alias Model =
    { bigR : Int
    , smallR : Int
    , pen : Float
    , samples : Int
    , stroke : String
    }


viz : Viz
viz =
    { kind = "spirograph"
    , name = "Spirograph"
    , description = "The looping hypotrochoid of a circle rolling inside a circle."
    , about = "The Spirograph, patented as a drawing toy in 1965, traces a \"hypotrochoid\": fix a pen in a small gear and roll it around the inside of a larger ring. The curve loops back on itself, and how many petals it makes is decided by the ratio of the two radii.\n\nThe same epicyclic curves have a long mathematical pedigree — Dürer drew them in 1525, and Ptolemy's epicycles modelled the planets with circles rolling on circles. They are a tactile introduction to parametric curves and to how simple ratios govern periodic motion."
    , starter = toSource default
    , movable = False
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { bigR = 5, smallR = 3, pen = 0.8, samples = 1400, stroke = "#7cdcff" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map5 Model
                    (Value.intField "bigR" fs)
                    (Value.intField "smallR" fs)
                    (Value.numField "pen" fs)
                    (Value.intField "samples" fs)
                    (Value.strField "stroke" fs)
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"spirograph\"\n"
        ++ "    , bigR = "
        ++ String.fromInt d.bigR
        ++ "\n    , smallR = "
        ++ String.fromInt d.smallR
        ++ "\n    , pen = "
        ++ Value.numStr d.pen
        ++ "\n    , samples = "
        ++ String.fromInt d.samples
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"



-- RENDER ------------------------------------------------------------------------------------------


prepare : Model -> (String -> Float -> Svg msg)
prepare d =
    let
        bigR =
            toFloat (max 2 d.bigR)

        smallR =
            toFloat (clamp 1 (d.bigR - 1) d.smallR)

        diff =
            bigR - smallR

        samples =
            clamp 100 6000 d.samples

        -- close the curve: one period is smallR/gcd turns; oversampling to smallR turns is safe
        turns =
            toFloat (max 1 d.smallR)

        point i =
            let
                t =
                    2 * pi * turns * toFloat i / toFloat samples

                k =
                    diff / smallR
            in
            ( diff * cos t + d.pen * smallR * cos (k * t), diff * sin t - d.pen * smallR * sin (k * t) )

        raw =
            List.map point (List.range 0 samples)

        ( cx, cy, scale ) =
            Draw.fitTransform raw

        pts =
            List.map (\( x, y ) -> ( (x - cx) * scale, -(y - cy) * scale )) raw
    in
    \mode phase -> Draw.stage [ Draw.curve "1.2" (Color.resolve mode d.stroke) phase pts ]



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a spirograph yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Gears"
                    [ Form.slider "outer radius" 3 13 1 (toFloat d.bigR) (\v -> toSource { d | bigR = round v })
                    , Form.slider "inner radius" 1 12 1 (toFloat d.smallR) (\v -> toSource { d | smallR = round v })
                    , Form.slider "pen offset" 0.1 1 0.02 d.pen (\v -> toSource { d | pen = v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "5 · 3" (toSource { d | bigR = 5, smallR = 3 })
                        , Form.preset "7 · 4" (toSource { d | bigR = 7, smallR = 4 })
                        , Form.preset "8 · 3" (toSource { d | bigR = 8, smallR = 3 })
                        , Form.preset "11 · 7" (toSource { d | bigR = 11, smallR = 7 })
                        , Form.preset "13 · 5" (toSource { d | bigR = 13, smallR = 5 })
                        , Form.preset "Ring 9 · 8" (toSource { d | bigR = 9, smallR = 8 })
                        ]
                    ]
                ]
