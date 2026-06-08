module Viz.MaurerRose exposing (viz)

{-| **Maurer rose** — walk a polyline around the rose curve `r = sin(n·θ)`, but sampling θ in steps of
`d` **degrees**. When `d` is chosen well the straight chords weave a lattice of striking lines over
the petals. Two integers, a world of patterns. Self-contained: model, render and controls live here.
-}

import Draw
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as A
import Value
import Viz exposing (Viz)


type alias Model =
    { n : Int
    , d : Int
    , stroke : String
    }


viz : Viz
viz =
    { kind = "rose"
    , name = "Maurer rose"
    , description = "Chords around a rose curve weave a lattice."
    , starter = toSource default
    , movable = False
    , render = \source -> Result.map (\m -> always (view m)) (decode source)
    , controls = controls
    }


default : Model
default =
    { n = 6, d = 71, stroke = "#ff7ce0" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map3 Model
                    (Value.intField "n" fs)
                    (Value.intField "d" fs)
                    (Value.strField "stroke" fs)
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"rose\"\n"
        ++ "    , n = "
        ++ String.fromInt d.n
        ++ "\n    , d = "
        ++ String.fromInt d.d
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"



-- RENDER ------------------------------------------------------------------------------------------


view : Model -> Svg msg
view d =
    let
        scale =
            275

        point k =
            let
                theta =
                    toFloat (k * d.d) * pi / 180

                r =
                    sin (toFloat d.n * theta) * scale
            in
            Draw.r2 (r * cos theta) ++ "," ++ Draw.r2 (r * sin theta)

        pts =
            String.join " " (List.map point (List.range 0 360))
    in
    Draw.stage
        [ Svg.polyline
            [ A.points pts
            , A.fill "none"
            , A.stroke d.stroke
            , A.strokeWidth "1"
            , A.strokeLinejoin "round"
            , A.opacity "0.9"
            ]
            []
        ]



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a Maurer rose yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Curve"
                    [ Form.slider "n (petals)" 1 12 1 (toFloat d.n) (\v -> toSource { d | n = round v })
                    , Form.slider "d (degrees)" 1 179 1 (toFloat d.d) (\v -> toSource { d | d = round v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "6 · 71" (toSource { d | n = 6, d = 71 })
                        , Form.preset "2 · 39" (toSource { d | n = 2, d = 39 })
                        , Form.preset "5 · 97" (toSource { d | n = 5, d = 97 })
                        , Form.preset "7 · 19" (toSource { d | n = 7, d = 19 })
                        ]
                    ]
                ]
