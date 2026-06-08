module Viz.Bifurcation exposing (viz)

{-| **Bifurcation diagram** of the logistic map `x ↦ r·x·(1−x)`: for each growth rate `r` across a
range, settle the orbit and then plot the values it visits. The result is the classic period-doubling
road to chaos. Self-contained: model, decode/print, render and controls all live here.
-}

import Color
import Draw
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Value
import Viz exposing (Viz)


type alias Model =
    { rMin : Float
    , rMax : Float
    , columns : Int
    , iterations : Int
    , settle : Int
    , stroke : String
    }


viz : Viz
viz =
    { kind = "bifurcation"
    , name = "Bifurcation diagram"
    , description = "The logistic map's period-doubling route to chaos."
    , about = "The logistic map x ↦ r·x·(1−x) is a one-line model of population growth. Plotting the values an orbit eventually settles into, as the growth rate r increases, gives the bifurcation diagram: a single value splits into two, then four, then eight, faster and faster, before dissolving into chaos — with narrow windows of order reappearing inside it.\n\nIn the 1970s Mitchell Feigenbaum discovered that the period-doublings shrink by a universal constant (≈ 4.669…) found in countless unrelated systems, from dripping taps to electronics. It was decisive evidence that the route to chaos has a deep, shared mathematical structure."
    , starter = toSource default
    , movable = False
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { rMin = 2.5, rMax = 4, columns = 700, iterations = 120, settle = 200, stroke = "#67e8f9" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map5 (\rMin rMax columns iterations settle -> ( rMin, rMax, ( columns, iterations, settle ) ))
                    (Value.numField "rMin" fs)
                    (Value.numField "rMax" fs)
                    (Value.intField "columns" fs)
                    (Value.intField "iterations" fs)
                    (Value.intField "settle" fs)
                    |> Result.andThen
                        (\( rMin, rMax, ( columns, iterations, settle ) ) ->
                            Result.map (\stroke -> Model rMin rMax columns iterations settle stroke)
                                (Value.strField "stroke" fs)
                        )
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"bifurcation\"\n"
        ++ "    , rMin = "
        ++ Value.numStr d.rMin
        ++ "\n    , rMax = "
        ++ Value.numStr d.rMax
        ++ "\n    , columns = "
        ++ String.fromInt d.columns
        ++ "\n    , iterations = "
        ++ String.fromInt d.iterations
        ++ "\n    , settle = "
        ++ String.fromInt d.settle
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"



-- RENDER ------------------------------------------------------------------------------------------


{-| Iterate every column **once** into a dot path; the drawer only restrokes it. -}
prepare : Model -> (String -> Float -> Svg msg)
prepare d =
    let
        cols =
            clamp 50 1500 d.columns

        iters =
            clamp 10 400 d.iterations

        settle =
            clamp 0 1000 d.settle

        span =
            max 0.0001 (d.rMax - d.rMin)

        column i =
            let
                r =
                    d.rMin + span * toFloat i / toFloat (max 1 (cols - 1))

                px =
                    -280 + 560 * toFloat i / toFloat (max 1 (cols - 1))
            in
            -- settle the orbit, then emit a point per visited value
            orbit r (settle + iters) settle px

        pts =
            List.concatMap column (List.range 0 (cols - 1))
    in
    \mode phase -> Draw.stage [ Draw.cloud "0.7" (Color.resolve mode d.stroke) phase pts ]


{-| Iterate the logistic map `total` times from x=0.5, emitting `(px, screenY)` once past `settle`. -}
orbit : Float -> Int -> Int -> Float -> List ( Float, Float )
orbit r total settle px =
    let
        go step x acc =
            if step >= total then
                acc

            else
                let
                    nx =
                        r * x * (1 - x)

                    acc2 =
                        if step >= settle then
                            ( px, 280 - 560 * nx ) :: acc

                        else
                            acc
                in
                go (step + 1) nx acc2
    in
    go 0 0.5 []



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a bifurcation diagram yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Range"
                    [ Form.slider "r min" 0 4 0.01 d.rMin (\v -> toSource { d | rMin = v })
                    , Form.slider "r max" 0 4 0.01 d.rMax (\v -> toSource { d | rMax = v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Sampling"
                    [ Form.slider "columns" 100 1500 50 (toFloat d.columns) (\v -> toSource { d | columns = round v })
                    , Form.slider "iterations" 20 300 10 (toFloat d.iterations) (\v -> toSource { d | iterations = round v })
                    , Form.slider "settle" 0 500 10 (toFloat d.settle) (\v -> toSource { d | settle = round v })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Full 2.5–4" (toSource { d | rMin = 2.5, rMax = 4 })
                        , Form.preset "Doubling 3.4–3.6" (toSource { d | rMin = 3.4, rMax = 3.6 })
                        , Form.preset "Chaos 3.83–3.86" (toSource { d | rMin = 3.83, rMax = 3.86 })
                        , Form.preset "Window 3.625–3.635" (toSource { d | rMin = 3.625, rMax = 3.635 })
                        , Form.preset "Onset 3.56–3.58" (toSource { d | rMin = 3.56, rMax = 3.58 })
                        , Form.preset "Wide 2.8–4" (toSource { d | rMin = 2.8, rMax = 4 })
                        ]
                    ]
                ]
