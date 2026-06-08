module Viz.Chladni exposing (viz)

{-| **Chladni figure** — the nodal pattern of a vibrating square plate: where it doesn't move, sand
settles. For a simple mode this is the zero set of `f(x, y) = cos(n·πx)·cos(m·πy) − cos(m·πx)·cos(n·πy)`.
Cells near a node are filled into one `<path>`.
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
    { n : Int
    , m : Int
    , resolution : Int
    , stroke : String
    }


viz : Viz
viz =
    { kind = "chladni"
    , name = "Chladni figure"
    , description = "Where a vibrating plate stands still — the sand-pattern nodes."
    , starter = toSource default
    , movable = False
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { n = 3, m = 5, resolution = 170, stroke = "#e2e8f0" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map4 Model
                    (Value.intField "n" fs)
                    (Value.intField "m" fs)
                    (Value.intField "resolution" fs)
                    (Value.strField "stroke" fs)
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"chladni\"\n"
        ++ "    , n = "
        ++ String.fromInt d.n
        ++ "\n    , m = "
        ++ String.fromInt d.m
        ++ "\n    , resolution = "
        ++ String.fromInt d.resolution
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"



-- RENDER ------------------------------------------------------------------------------------------


prepare : Model -> (String -> Float -> Svg msg)
prepare d =
    let
        res =
            clamp 40 220 d.resolution

        nf =
            toFloat d.n

        mf =
            toFloat d.m

        cell =
            580 / toFloat res

        w =
            Draw.r1 (cell + 0.6)

        threshold =
            7 / toFloat res

        node i j =
            let
                x =
                    toFloat i / toFloat (res - 1)

                y =
                    toFloat j / toFloat (res - 1)

                f =
                    cos (nf * pi * x) * cos (mf * pi * y) - cos (mf * pi * x) * cos (nf * pi * y)
            in
            abs f < threshold

        rect i j =
            "M" ++ Draw.r1 (-290 + toFloat i * cell) ++ " " ++ Draw.r1 (-290 + toFloat j * cell) ++ "h" ++ w ++ "v" ++ w ++ "h-" ++ w ++ "z"

        path =
            String.concat
                (List.concatMap
                    (\j -> List.filterMap (\i -> if node i j then Just (rect i j) else Nothing) (List.range 0 (res - 1)))
                    (List.range 0 (res - 1))
                )
    in
    \mode phase ->
        Draw.stage [ Svg.path [ A.d path, A.fill (Color.solid (Color.resolve mode d.stroke) phase), A.opacity "0.95" ] [] ]



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a Chladni figure yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Mode"
                    [ Form.slider "n" 1 12 1 (toFloat d.n) (\v -> toSource { d | n = round v })
                    , Form.slider "m" 1 12 1 (toFloat d.m) (\v -> toSource { d | m = round v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Quality"
                    [ Form.slider "resolution" 60 220 2 (toFloat d.resolution) (\v -> toSource { d | resolution = round v })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "3 · 5" (toSource { d | n = 3, m = 5 })
                        , Form.preset "4 · 7" (toSource { d | n = 4, m = 7 })
                        , Form.preset "1 · 6" (toSource { d | n = 1, m = 6 })
                        , Form.preset "5 · 8" (toSource { d | n = 5, m = 8 })
                        , Form.preset "2 · 3" (toSource { d | n = 2, m = 3 })
                        , Form.preset "6 · 9" (toSource { d | n = 6, m = 9 })
                        , Form.preset "3 · 8" (toSource { d | n = 3, m = 8 })
                        ]
                    ]
                ]
