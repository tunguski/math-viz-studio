module Viz.RandomWalk exposing (viz)

{-| **Random walk** — a path that steps a fixed distance in a uniformly random direction each tick (a
discrete Brownian motion). Deterministic here: a small seeded generator drives the angles, so a given
`seed` always draws the same wander. A `gradient` colouring traces time along the path.
-}

import Color
import Draw
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Value
import Viz exposing (Viz)


type alias Model =
    { steps : Int
    , seed : Int
    , stepSize : Float
    , stroke : String
    }


viz : Viz
viz =
    { kind = "walk"
    , name = "Random walk"
    , description = "A drunkard's wander — discrete Brownian motion."
    , about = "A random walk takes one step of fixed length in a random direction, over and over. Karl Pearson posed it in 1905 as the \"drunkard's walk\", and it turns out to model an enormous range of phenomena: a pollen grain jostled by water molecules (Brownian motion), a diffusing gas, a fluctuating share price.\n\nIts statistics are deep and surprising — in two dimensions a walk is certain to return to its start eventually, but in three it may wander off forever (\"a drunk man will find his way home, but a drunk bird may not\"). As the step shrinks, the jagged path converges to Brownian motion, a cornerstone of probability and mathematical finance."
    , starter = toSource default
    , movable = False
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { steps = 4000, seed = 7, stepSize = 6, stroke = "#67e8f9" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map4 Model
                    (Value.intField "steps" fs)
                    (Value.intField "seed" fs)
                    (Value.numField "stepSize" fs)
                    (Value.strField "stroke" fs)
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"walk\"\n"
        ++ "    , steps = "
        ++ String.fromInt d.steps
        ++ "\n    , seed = "
        ++ String.fromInt d.seed
        ++ "\n    , stepSize = "
        ++ Value.numStr d.stepSize
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"



-- RENDER ------------------------------------------------------------------------------------------


prepare : Model -> (String -> Float -> Svg msg)
prepare d =
    let
        steps =
            clamp 50 30000 d.steps

        walk i ( seed, x, y, acc ) =
            let
                seed2 =
                    modBy 2147483647 (48271 * seed)

                angle =
                    toFloat seed2 / 2147483647 * 2 * pi

                nx =
                    x + cos angle * d.stepSize

                ny =
                    y + sin angle * d.stepSize
            in
            ( seed2, nx, ny, ( nx, ny ) :: acc )

        ( _, _, _, raw ) =
            List.foldl walk ( max 1 d.seed, 0, 0, [ ( 0, 0 ) ] ) (List.range 1 steps)

        ( cx, cy, scale ) =
            Draw.fitTransform raw

        pts =
            List.map (\( x, y ) -> ( (x - cx) * scale, -(y - cy) * scale )) (List.reverse raw)
    in
    \mode phase -> Draw.stage [ Draw.curve "1.1" (Color.resolve mode d.stroke) phase pts ]



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a random walk yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Walk"
                    [ Form.slider "steps" 100 30000 100 (toFloat d.steps) (\v -> toSource { d | steps = round v })
                    , Form.slider "seed" 1 200 1 (toFloat d.seed) (\v -> toSource { d | seed = round v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Seeds"
                    [ Form.presets
                        [ Form.preset "Seed 7" (toSource { d | seed = 7 })
                        , Form.preset "Seed 23" (toSource { d | seed = 23 })
                        , Form.preset "Seed 42" (toSource { d | seed = 42 })
                        , Form.preset "Seed 99" (toSource { d | seed = 99 })
                        , Form.preset "Long wander" (toSource { d | steps = 12000, seed = 5 })
                        , Form.preset "Tight" (toSource { d | steps = 8000, stepSize = 3 })
                        ]
                    ]
                , Form.hint "Try the Gradient colour mode to follow the walk through time."
                ]
