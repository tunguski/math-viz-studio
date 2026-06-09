module Viz.Lissajous exposing (viz)

{-| **Lissajous figure** — the curve `x = sin(a·t + δ)`, `y = sin(b·t)` traced by two perpendicular
oscillations. The frequency ratio a : b fixes the shape; the phase δ rotates and folds it.
-}

import Color
import Draw
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Value
import Viz exposing (Viz)


type alias Model =
    { freqX : Int
    , freqY : Int
    , phase : Float
    , samples : Int
    , stroke : String
    }


viz : Viz
viz =
    { kind = "lissajous"
    , name = "Lissajous figure"
    , description = "Two perpendicular oscillations weave a curve."
    , about = "A Lissajous figure is what you get when two harmonic oscillations are set at right angles — horizontal x = sin(a·t + δ) against vertical y = sin(b·t). Nathaniel Bowditch drew them in 1815 and Jules Lissajous studied them with mirrors and tuning forks in 1857.\n\nThe ratio of the two frequencies decides the shape — a 1:1 makes an ellipse, 3:2 a pretzel — and the phase rotates it. On an oscilloscope they're the classic way to compare two signals' frequency and phase, and they're the simple cousin of the harmonograph."
    , starter = toSource default
    , movable = False
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { freqX = 3, freqY = 2, phase = 1.5708, samples = 1200, stroke = "#7cdcff" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map5 Model
                    (Value.intField "freqX" fs)
                    (Value.intField "freqY" fs)
                    (Value.numField "phase" fs)
                    (Value.intField "samples" fs)
                    (Value.strField "stroke" fs)
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"lissajous\"\n    , freqX = "
        ++ String.fromInt d.freqX
        ++ "\n    , freqY = "
        ++ String.fromInt d.freqY
        ++ "\n    , phase = "
        ++ Value.numStr d.phase
        ++ "\n    , samples = "
        ++ String.fromInt d.samples
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"



-- RENDER ------------------------------------------------------------------------------------------


prepare : Model -> (String -> Float -> Svg msg)
prepare d =
    let
        samples =
            clamp 100 4000 d.samples

        point i =
            let
                t =
                    2 * pi * toFloat i / toFloat samples
            in
            ( 275 * sin (toFloat d.freqX * t + d.phase), 275 * sin (toFloat d.freqY * t) )

        pts =
            List.map point (List.range 0 samples)
    in
    \mode phase -> Draw.stage [ Draw.curve "1.6" (Color.resolve mode d.stroke) phase pts ]



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a Lissajous figure yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Oscillation"
                    [ Form.slider "x frequency" 1 12 1 (toFloat d.freqX) (\v -> toSource { d | freqX = round v })
                    , Form.slider "y frequency" 1 12 1 (toFloat d.freqY) (\v -> toSource { d | freqY = round v })
                    , Form.slider "phase" 0 6.2832 0.01 d.phase (\v -> toSource { d | phase = v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "1 : 1" (toSource { d | freqX = 1, freqY = 1 })
                        , Form.preset "1 : 2" (toSource { d | freqX = 1, freqY = 2 })
                        , Form.preset "3 : 2" (toSource { d | freqX = 3, freqY = 2 })
                        , Form.preset "3 : 4" (toSource { d | freqX = 3, freqY = 4 })
                        , Form.preset "5 : 4" (toSource { d | freqX = 5, freqY = 4 })
                        , Form.preset "5 : 6" (toSource { d | freqX = 5, freqY = 6 })
                        ]
                    ]
                ]
