module Viz.Rossler exposing (viz)

{-| **Rössler attractor** — the flow `x' = −y − z`, `y' = x + a·y`, `z' = b + z·(x − c)`,
Euler-integrated and projected. A simpler cousin of the Lorenz system: a flat spiralling sheet with a
single fold. Spun by yaw, which the animation clock drives.
-}

import Color
import Draw exposing (Vec3)
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Value
import Viz exposing (Viz)


type alias Model =
    { a : Float
    , b : Float
    , c : Float
    , dt : Float
    , steps : Int
    , yaw : Float
    , pitch : Float
    , stroke : String
    }


viz : Viz
viz =
    { kind = "rossler"
    , name = "Rössler attractor"
    , description = "A chaotic flow — a spiralling sheet with a single fold."
    , about = "Otto Rössler designed his attractor in 1976 deliberately as the simplest possible chaotic flow — three equations with a single quadratic nonlinearity. A point spirals outward on a nearly flat sheet, then the lone fold lifts and folds it back to the centre, over and over.\n\nThat stretch-and-fold is the essence of chaos, and because Rössler's system is so spare it became a standard textbook companion to the Lorenz attractor for showing how deterministic equations produce unpredictable, never-repeating motion."
    , starter = toSource default
    , movable = True
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { a = 0.2, b = 0.2, c = 5.7, dt = 0.03, steps = 9000, yaw = 0.5, pitch = 1.0, stroke = "#86efac" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map5 (\a b c dt steps -> ( a, b, ( c, dt, steps ) ))
                    (Value.numField "a" fs)
                    (Value.numField "b" fs)
                    (Value.numField "c" fs)
                    (Value.numField "dt" fs)
                    (Value.intField "steps" fs)
                    |> Result.andThen
                        (\( a, b, ( c, dt, steps ) ) ->
                            Result.map3 (\yaw pitch stroke -> Model a b c dt steps yaw pitch stroke)
                                (Value.numField "yaw" fs)
                                (Value.numField "pitch" fs)
                                (Value.strField "stroke" fs)
                        )
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"rossler\"\n"
        ++ "    , a = "
        ++ Value.numStr d.a
        ++ "\n    , b = "
        ++ Value.numStr d.b
        ++ "\n    , c = "
        ++ Value.numStr d.c
        ++ "\n    , dt = "
        ++ Value.numStr d.dt
        ++ "\n    , steps = "
        ++ String.fromInt d.steps
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
        path3 =
            integrate d

        c =
            Draw.centroid path3

        centered =
            List.map (\p -> { x = p.x - c.x, y = p.y - c.y, z = p.z - c.z }) path3
    in
    \mode phase -> draw d centered mode phase


draw : Model -> List Vec3 -> String -> Float -> Svg msg
draw d centered mode phase =
    let
        proj =
            List.map (Draw.rotate2 (d.yaw + phase) d.pitch) centered

        ( cx, cy, scale ) =
            Draw.fitTransform proj

        screen ( x, y ) =
            ( (x - cx) * scale, -(y - cy) * scale )
    in
    Draw.stage [ Draw.curve "1" (Color.resolve mode d.stroke) phase (List.map screen proj) ]


integrate : Model -> List Vec3
integrate d =
    let
        steps =
            clamp 100 20000 d.steps

        step _ ( p, acc ) =
            let
                np =
                    { x = p.x + (-p.y - p.z) * d.dt
                    , y = p.y + (p.x + d.a * p.y) * d.dt
                    , z = p.z + (d.b + p.z * (p.x - d.c)) * d.dt
                    }
            in
            ( np, np :: acc )

        ( _, pts ) =
            List.foldl step ( { x = 1, y = 1, z = 1 }, [] ) (List.range 1 steps)
    in
    List.reverse pts



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a Rössler attractor yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Flow"
                    [ Form.slider "a" 0 0.4 0.005 d.a (\v -> toSource { d | a = v })
                    , Form.slider "b" 0 2 0.01 d.b (\v -> toSource { d | b = v })
                    , Form.slider "c" 2 12 0.05 d.c (\v -> toSource { d | c = v })
                    ]
                , Form.group "Integration"
                    [ Form.slider "dt" 0.005 0.06 0.001 d.dt (\v -> toSource { d | dt = v })
                    , Form.slider "steps" 1000 16000 500 (toFloat d.steps) (\v -> toSource { d | steps = round v })
                    ]
                , Form.group "View"
                    [ Form.slider "pitch" 0 6.2832 0.01 d.pitch (\v -> toSource { d | pitch = v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Classic" (toSource { d | a = 0.2, b = 0.2, c = 5.7 })
                        , Form.preset "Period-2" (toSource { d | a = 0.2, b = 0.2, c = 4 })
                        , Form.preset "Banded" (toSource { d | a = 0.1, b = 0.1, c = 14 })
                        , Form.preset "Spiral c=9" (toSource { d | a = 0.2, b = 0.2, c = 9 })
                        , Form.preset "Funnel c=18" (toSource { d | a = 0.2, b = 0.2, c = 18 })
                        , Form.preset "Thin a=0.1" (toSource { d | a = 0.1, b = 0.2, c = 5.7 })
                        ]
                    ]
                ]
