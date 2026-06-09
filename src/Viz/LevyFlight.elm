module Viz.LevyFlight exposing (viz)

{-| **Lévy flight** — a random walk whose step lengths follow a heavy-tailed power law, so long
stretches of small steps are punctuated by rare, dramatic jumps. The exponent `alpha` controls how
heavy the tail is (smaller = wilder jumps).
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
    , alpha : Float
    , stroke : String
    }


viz : Viz
viz =
    { kind = "levy"
    , name = "Lévy flight"
    , description = "A random walk with rare, dramatic long jumps."
    , about = "A Lévy flight is a random walk whose step lengths come from a heavy-tailed distribution: mostly small moves, but with a non-negligible chance of an enormous leap. Unlike ordinary Brownian motion it has no characteristic scale, and it covers ground far more efficiently — clusters of local exploration linked by long hops.\n\nNamed after Paul Lévy, these walks model an astonishing range of search behaviour: the foraging paths of albatrosses, sharks and bees, the spread of epidemics and bank notes, and even patterns in human travel. The exponent here tunes how wild the jumps are."
    , starter = toSource default
    , movable = False
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { steps = 900, seed = 7, alpha = 1.5, stroke = "#fca5f1" }



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
                    (Value.numField "alpha" fs)
                    (Value.strField "stroke" fs)
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"levy\"\n    , steps = "
        ++ String.fromInt d.steps
        ++ "\n    , seed = "
        ++ String.fromInt d.seed
        ++ "\n    , alpha = "
        ++ Value.numStr d.alpha
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"



-- RENDER ------------------------------------------------------------------------------------------


prepare : Model -> (String -> Float -> Svg msg)
prepare d =
    let
        steps =
            clamp 50 8000 d.steps

        alpha =
            clamp 0.5 3 d.alpha

        lcg s =
            modBy 2147483647 (48271 * s)

        step _ ( seed, x, y, acc ) =
            let
                s1 =
                    lcg seed

                s2 =
                    lcg s1

                angle =
                    toFloat s1 / 2147483647 * 2 * pi

                u =
                    (toFloat s2 + 1) / 2147483648

                len =
                    clamp 1 90 (u ^ (-1 / alpha))

                nx =
                    x + cos angle * len

                ny =
                    y + sin angle * len
            in
            ( s2, nx, ny, ( nx, ny ) :: acc )

        ( _, _, _, raw ) =
            List.foldl step ( max 1 d.seed, 0, 0, [ ( 0, 0 ) ] ) (List.range 1 steps)

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
            Form.note ("This file isn't a Lévy flight yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Flight"
                    [ Form.slider "steps" 100 8000 50 (toFloat d.steps) (\v -> toSource { d | steps = round v })
                    , Form.slider "alpha (tail)" 0.6 3 0.05 d.alpha (\v -> toSource { d | alpha = v })
                    , Form.slider "seed" 1 200 1 (toFloat d.seed) (\v -> toSource { d | seed = round v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Wild α=1.2" (toSource { d | alpha = 1.2 })
                        , Form.preset "Classic α=1.5" (toSource { d | alpha = 1.5 })
                        , Form.preset "Mild α=2.2" (toSource { d | alpha = 2.2 })
                        , Form.preset "Seed 23" (toSource { d | seed = 23 })
                        , Form.preset "Seed 50" (toSource { d | seed = 50 })
                        , Form.preset "Long" (toSource { d | steps = 3000 })
                        ]
                    ]
                , Form.hint "Try the Gradient colour mode to follow the flight through time."
                ]
