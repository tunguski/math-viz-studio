module Viz.Henon exposing (viz)

{-| **Hénon map** — the discrete dynamical system `xₙ₊₁ = 1 − a·xₙ² + yₙ`, `yₙ₊₁ = b·xₙ`. For the
classic `a = 1.4, b = 0.3` the orbit settles onto a thin, banded strange attractor with fractal
cross-section.
-}

import Color
import Draw
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Value
import Viz exposing (Viz)


type alias Model =
    { a : Float
    , b : Float
    , points : Int
    , stroke : String
    }


viz : Viz
viz =
    { kind = "henon"
    , name = "Hénon map"
    , description = "A thin banded strange attractor from a 2-D map."
    , about = "In 1976 the astronomer Michel Hénon sought the simplest map that would still show the stretch-and-fold dynamics of chaos, and arrived at this two-line quadratic recurrence. For a = 1.4, b = 0.3 its orbit converges onto a slender, boomerang-shaped attractor.\n\nZoom in and the apparently smooth curves reveal themselves as bundles of curves, then bundles of bundles — a fractal cross-section, like a Cantor set. The Hénon map became one of the most-studied examples in dynamical systems, a discrete-time companion to the Lorenz attractor."
    , starter = toSource default
    , movable = False
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { a = 1.4, b = 0.3, points = 30000, stroke = "#fbbf24" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map4 Model
                    (Value.numField "a" fs)
                    (Value.numField "b" fs)
                    (Value.intField "points" fs)
                    (Value.strField "stroke" fs)
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"henon\"\n    , a = "
        ++ Value.numStr d.a
        ++ "\n    , b = "
        ++ Value.numStr d.b
        ++ "\n    , points = "
        ++ String.fromInt d.points
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"



-- RENDER ------------------------------------------------------------------------------------------


prepare : Model -> (String -> Float -> Svg msg)
prepare d =
    let
        raw =
            orbit d (clamp 1000 60000 d.points)

        ( cx, cy, scale ) =
            Draw.fitTransform raw

        pts =
            List.map (\( x, y ) -> ( (x - cx) * scale, -(y - cy) * scale )) raw
    in
    \mode phase -> Draw.stage [ Draw.cloud "0.7" (Color.resolve mode d.stroke) phase pts ]


orbit : Model -> Int -> List ( Float, Float )
orbit d count =
    let
        step _ ( x, y, acc ) =
            let
                nx =
                    1 - d.a * x * x + y

                ny =
                    d.b * x
            in
            ( nx, ny, ( nx, ny ) :: acc )

        ( _, _, pts ) =
            List.foldl step ( 0, 0, [] ) (List.range 1 count)
    in
    List.drop 20 (List.reverse pts)



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a Hénon map yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Parameters"
                    [ Form.slider "a" 0.8 1.45 0.005 d.a (\v -> toSource { d | a = v })
                    , Form.slider "b" 0.1 0.4 0.005 d.b (\v -> toSource { d | b = v })
                    , Form.slider "points" 5000 60000 1000 (toFloat d.points) (\v -> toSource { d | points = round v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Classic" (toSource { d | a = 1.4, b = 0.3 })
                        , Form.preset "a=1.2" (toSource { d | a = 1.2, b = 0.3 })
                        , Form.preset "a=1.06" (toSource { d | a = 1.06, b = 0.3 })
                        , Form.preset "b=0.2" (toSource { d | a = 1.4, b = 0.2 })
                        , Form.preset "Curl" (toSource { d | a = 1.0, b = 0.35 })
                        ]
                    ]
                ]
