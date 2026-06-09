module Viz.Truchet exposing (viz)

{-| **Truchet tiles** — fill a grid with a single tile (here a pair of quarter-circle arcs joining
the midpoints of adjacent edges), each placed in one of two random orientations. The arcs link across
tiles into flowing, maze-like loops. Deterministic: the `seed` fixes the tiling.
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
    { size : Int
    , seed : Int
    , stroke : String
    }


viz : Viz
viz =
    { kind = "truchet"
    , name = "Truchet tiles"
    , description = "One tile, two orientations, flowing maze-like loops."
    , about = "In 1704 Father Sébastien Truchet studied all the patterns you can make from a square tile split diagonally into a light and a dark half, simply by rotating it. The idea was revived and broadened by Cyril Stanley Smith in 1987.\n\nThe quarter-circle version shown here — each tile carrying two arcs — links up across the grid into smooth, interlocking loops, so a field of independent random choices produces a coherent maze. Truchet tilings are a staple of generative art and a neat illustration of order emerging from local randomness."
    , starter = toSource default
    , movable = False
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { size = 12, seed = 7, stroke = "#67e8f9" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map3 Model
                    (Value.intField "size" fs)
                    (Value.intField "seed" fs)
                    (Value.strField "stroke" fs)
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"truchet\"\n    , size = "
        ++ String.fromInt d.size
        ++ "\n    , seed = "
        ++ String.fromInt d.seed
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"



-- RENDER ------------------------------------------------------------------------------------------


prepare : Model -> (String -> Float -> Svg msg)
prepare d =
    let
        size =
            clamp 3 40 d.size

        cs =
            580 / toFloat size

        r =
            cs / 2

        tile t ( seed, acc ) =
            let
                seed2 =
                    modBy 2147483647 (48271 * seed)

                i =
                    modBy size t

                j =
                    t // size

                ox =
                    -290 + toFloat i * cs

                oy =
                    -290 + toFloat j * cs

                arcs =
                    if modBy 2 seed2 == 0 then
                        arc ( ox + r, oy ) ( ox, oy + r ) r ++ arc ( ox + cs, oy + r ) ( ox + r, oy + cs ) r

                    else
                        arc ( ox + r, oy ) ( ox + cs, oy + r ) r ++ arc ( ox, oy + r ) ( ox + r, oy + cs ) r
            in
            ( seed2, acc ++ arcs )

        ( _, path ) =
            List.foldl tile ( max 1 d.seed, "" ) (List.range 0 (size * size - 1))
    in
    \mode phase ->
        Draw.stage
            [ Svg.path
                [ A.d path, A.fill "none", A.stroke (Color.solid (Color.resolve mode d.stroke) phase), A.strokeWidth "2", A.strokeLinecap "round" ]
                []
            ]


arc : ( Float, Float ) -> ( Float, Float ) -> Float -> String
arc ( x1, y1 ) ( x2, y2 ) r =
    "M" ++ Draw.r1 x1 ++ " " ++ Draw.r1 y1 ++ "A" ++ Draw.r1 r ++ " " ++ Draw.r1 r ++ " 0 0 0 " ++ Draw.r1 x2 ++ " " ++ Draw.r1 y2



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a Truchet tiling yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Tiling"
                    [ Form.slider "grid size" 3 40 1 (toFloat d.size) (\v -> toSource { d | size = round v })
                    , Form.slider "seed" 1 200 1 (toFloat d.seed) (\v -> toSource { d | seed = round v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Seed 7" (toSource { d | seed = 7 })
                        , Form.preset "Seed 19" (toSource { d | seed = 19 })
                        , Form.preset "Seed 88" (toSource { d | seed = 88 })
                        , Form.preset "Coarse" (toSource { d | size = 6 })
                        , Form.preset "Fine" (toSource { d | size = 24 })
                        , Form.preset "Dense" (toSource { d | size = 34, seed = 42 })
                        ]
                    ]
                ]
