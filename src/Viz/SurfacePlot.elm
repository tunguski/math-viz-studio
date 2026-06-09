module Viz.SurfacePlot exposing (viz)

{-| **Surface plot** — the graph of a height function `z = f(x, y)` over a square, drawn as a rotating
wireframe. Switch `fn` to see classic shapes: the saddle `x² − y²`, the monkey saddle, ripples, peaks
and waves.
-}

import Color
import Draw exposing (Vec3)
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Value
import Viz exposing (Viz)


type alias Model =
    { fn : String
    , warp : Float
    , segU : Int
    , segV : Int
    , yaw : Float
    , pitch : Float
    , stroke : String
    }


viz : Viz
viz =
    { kind = "heightfield"
    , name = "Surface plot"
    , description = "The graph of z = f(x, y) — saddles, ripples and peaks."
    , about = "Plotting z = f(x, y) as a surface above the plane is the everyday way to picture a function of two variables — the staple of multivariable calculus, optimisation and data visualisation. Hills are maxima, basins are minima, and a saddle is a point that is a minimum one way and a maximum the other.\n\nThe saddle x² − y² and its relative the \"monkey saddle\" x³ − 3xy² (with a third dip, for the tail) are the textbook examples of critical points that are neither peaks nor valleys — central to understanding how surfaces curve."
    , starter = toSource default
    , movable = True
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { fn = "saddle", warp = 1, segU = 22, segV = 22, yaw = 0.6, pitch = 1.0, stroke = "#67e8f9" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map5 (\fn warp segU segV yaw -> ( fn, warp, ( segU, segV, yaw ) ))
                    (Value.strField "fn" fs)
                    (Value.numField "warp" fs)
                    (Value.intField "segU" fs)
                    (Value.intField "segV" fs)
                    (Value.numField "yaw" fs)
                    |> Result.andThen
                        (\( fn, warp, ( segU, segV, yaw ) ) ->
                            Result.map2 (\pitch stroke -> Model fn warp segU segV yaw pitch stroke)
                                (Value.numField "pitch" fs)
                                (Value.strField "stroke" fs)
                        )
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"heightfield\"\n    , fn = \""
        ++ d.fn
        ++ "\"\n    , warp = "
        ++ Value.numStr d.warp
        ++ "\n    , segU = "
        ++ String.fromInt d.segU
        ++ "\n    , segV = "
        ++ String.fromInt d.segV
        ++ "\n    , yaw = "
        ++ Value.numStr d.yaw
        ++ "\n    , pitch = "
        ++ Value.numStr d.pitch
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"



-- RENDER ------------------------------------------------------------------------------------------


prepare : Model -> (String -> Float -> Svg msg)
prepare d =
    let
        us =
            clamp 6 40 d.segU

        vs =
            clamp 6 40 d.segV

        ext =
            1.5

        point ui vj =
            let
                x =
                    -ext + 2 * ext * toFloat ui / toFloat (us - 1)

                y =
                    -ext + 2 * ext * toFloat vj / toFloat (vs - 1)
            in
            { x = x, y = y, z = d.warp * height d.fn x y }

        grid =
            List.concatMap (\ui -> List.map (point ui) (List.range 0 (vs - 1))) (List.range 0 (us - 1))
    in
    \mode phase -> Draw.surface (d.yaw + phase) d.pitch us vs False False grid (Color.solid (Color.resolve mode d.stroke) phase)


height : String -> Float -> Float -> Float
height fn x y =
    case fn of
        "saddle" ->
            x * x - y * y

        "monkey" ->
            x * x * x - 3 * x * y * y

        "ripple" ->
            let
                r =
                    sqrt (x * x + y * y)
            in
            cos (3 * r) * 0.8

        "waves" ->
            sin (3 * x) * cos (3 * y) * 0.7

        "peak" ->
            2 * e ^ (-(x * x + y * y) * 2) - 0.6

        "dome" ->
            1 - x * x - y * y

        _ ->
            0



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a surface plot yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Function"
                    [ Form.presets
                        [ Form.preset "Saddle" (toSource { d | fn = "saddle" })
                        , Form.preset "Monkey" (toSource { d | fn = "monkey" })
                        , Form.preset "Ripple" (toSource { d | fn = "ripple" })
                        , Form.preset "Waves" (toSource { d | fn = "waves" })
                        , Form.preset "Peak" (toSource { d | fn = "peak" })
                        , Form.preset "Dome" (toSource { d | fn = "dome" })
                        ]
                    ]
                , Form.group "Display"
                    [ Form.slider "height" 0.2 2.5 0.05 d.warp (\v -> toSource { d | warp = v })
                    , Form.slider "mesh" 6 40 1 (toFloat d.segU) (\v -> toSource { d | segU = round v, segV = round v })
                    , Form.slider "pitch" 0 6.2832 0.01 d.pitch (\v -> toSource { d | pitch = v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                ]
