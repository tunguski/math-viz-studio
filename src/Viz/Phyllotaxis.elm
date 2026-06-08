module Viz.Phyllotaxis exposing (viz)

{-| **Phyllotaxis** — Vogel's model of the floret spiral of a sunflower: the `n`-th seed sits at
radius `spread·√n` and angle `n·angle`. At the golden angle (≈137.5°) the seeds pack into the
familiar interleaved spirals. Each seed is its own dot, so a `gradient` colouring paints the spiral
order across the head.
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
    , angle : Float
    , spread : Float
    , stroke : String
    }


viz : Viz
viz =
    { kind = "phyllotaxis"
    , name = "Phyllotaxis"
    , description = "A sunflower's golden-angle seed spiral."
    , starter = toSource default
    , movable = False
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { count = 900, angle = 137.5, spread = 9, stroke = "#fcd34d" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map4 Model
                    (Value.intField "count" fs)
                    (Value.numField "angle" fs)
                    (Value.numField "spread" fs)
                    (Value.strField "stroke" fs)
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"phyllotaxis\"\n"
        ++ "    , count = "
        ++ String.fromInt d.count
        ++ "\n    , angle = "
        ++ Value.numStr d.angle
        ++ "\n    , spread = "
        ++ Value.numStr d.spread
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"



-- RENDER ------------------------------------------------------------------------------------------


prepare : Model -> (String -> Float -> Svg msg)
prepare d =
    let
        count =
            clamp 50 3000 d.count

        seed n =
            let
                r =
                    d.spread * sqrt (toFloat n)

                theta =
                    degrees (toFloat n * d.angle)
            in
            ( r * cos theta, r * sin theta )

        seeds =
            List.map seed (List.range 1 count)
    in
    \mode phase ->
        let
            coloring =
                Color.resolve mode d.stroke

            dot i ( x, y ) =
                Svg.circle
                    [ A.cx (Draw.r2 x)
                    , A.cy (Draw.r2 y)
                    , A.r "3.4"
                    , A.fill (Color.sample coloring (toFloat i / toFloat count) phase)
                    ]
                    []
        in
        Draw.stage (List.indexedMap dot seeds)



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a phyllotaxis yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Spiral"
                    [ Form.slider "seeds" 50 2000 10 (toFloat d.count) (\v -> toSource { d | count = round v })
                    , Form.slider "angle (°)" 130 145 0.1 d.angle (\v -> toSource { d | angle = v })
                    , Form.slider "spread" 4 16 0.2 d.spread (\v -> toSource { d | spread = v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Golden 137.5°" (toSource { d | angle = 137.5 })
                        , Form.preset "137.3°" (toSource { d | angle = 137.3 })
                        , Form.preset "137.6°" (toSource { d | angle = 137.6 })
                        , Form.preset "Fermat 99.5°" (toSource { d | angle = 99.5 })
                        , Form.preset "Loose" (toSource { d | spread = 13 })
                        , Form.preset "Dense" (toSource { d | spread = 6, count = 1500 })
                        ]
                    ]
                , Form.hint "Try the Gradient colour mode to paint the spiral order."
                ]
