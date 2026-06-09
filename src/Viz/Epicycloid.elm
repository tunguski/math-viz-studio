module Viz.Epicycloid exposing (viz)

{-| **Epicycloid** — the curve traced by a point on a circle of radius `smallR` rolling around the
*outside* of a fixed circle of radius `bigR`. Ratios give famous shapes: 1:1 the cardioid, 2:1 the
nephroid. (A circle rolling *inside* gives the related hypotrochoid of the Spirograph.)
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
    , samples : Int
    , stroke : String
    }


viz : Viz
viz =
    { kind = "epicycloid"
    , name = "Epicycloid"
    , description = "A point on a circle rolling around another circle."
    , about = "Roll a circle around the outside of a fixed one and watch a marked point on its rim: the looping curve it traces is an epicycloid. A wheel the same size gives the heart-shaped cardioid; twice as big gives the nephroid (\"kidney\"); larger ratios add more cusps.\n\nEpicycloids and their epicyclic cousins have a long history in astronomy — Ptolemy and his successors modelled planetary motion as circles rolling on circles for over a thousand years — and they live on in gear design and, of course, the Spirograph."
    , starter = toSource default
    , movable = False
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { bigR = 5, smallR = 3, samples = 1200, stroke = "#ff9cee" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map4 Model
                    (Value.intField "bigR" fs)
                    (Value.intField "smallR" fs)
                    (Value.intField "samples" fs)
                    (Value.strField "stroke" fs)
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"epicycloid\"\n    , bigR = "
        ++ String.fromInt d.bigR
        ++ "\n    , smallR = "
        ++ String.fromInt d.smallR
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
            toFloat (max 1 d.bigR)

        smallR =
            toFloat (max 1 d.smallR)

        samples =
            clamp 100 6000 d.samples

        turns =
            toFloat (max 1 d.smallR)

        k =
            (bigR + smallR) / smallR

        point i =
            let
                t =
                    2 * pi * turns * toFloat i / toFloat samples
            in
            ( (bigR + smallR) * cos t - smallR * cos (k * t), (bigR + smallR) * sin t - smallR * sin (k * t) )

        raw =
            List.map point (List.range 0 samples)

        ( cx, cy, scale ) =
            Draw.fitTransform raw

        pts =
            List.map (\( x, y ) -> ( (x - cx) * scale, -(y - cy) * scale )) raw
    in
    \mode phase -> Draw.stage [ Draw.curve "1.4" (Color.resolve mode d.stroke) phase pts ]



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't an epicycloid yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Circles"
                    [ Form.slider "fixed radius" 1 12 1 (toFloat d.bigR) (\v -> toSource { d | bigR = round v })
                    , Form.slider "rolling radius" 1 9 1 (toFloat d.smallR) (\v -> toSource { d | smallR = round v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Cardioid 1·1" (toSource { d | bigR = 1, smallR = 1 })
                        , Form.preset "Nephroid 2·1" (toSource { d | bigR = 2, smallR = 1 })
                        , Form.preset "3·1" (toSource { d | bigR = 3, smallR = 1 })
                        , Form.preset "5·1" (toSource { d | bigR = 5, smallR = 1 })
                        , Form.preset "5·3" (toSource { d | bigR = 5, smallR = 3 })
                        , Form.preset "7·2" (toSource { d | bigR = 7, smallR = 2 })
                        ]
                    ]
                ]
