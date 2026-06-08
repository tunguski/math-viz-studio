module Viz.Julia exposing (viz)

{-| **Julia set** of `z ↦ z² + c` — colour each point of the plane by how fast its orbit escapes (or
the in-set colour if it never does). The complex constant `c = cRe + cIm·i` is what you "build": move
it and the fractal morphs from a disc to dendrites to spirals.

To keep the DOM small, cells are bucketed by escape time and each bucket is drawn as a single filled
`<path>` — so the whole grid is a few dozen nodes, not tens of thousands.
-}

import Dict exposing (Dict)
import Draw
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as A
import Value
import Viz exposing (Viz)


type alias Model =
    { cRe : Float
    , cIm : Float
    , maxIter : Int
    , resolution : Int
    , hue : Float
    , stroke : String
    }


viz : Viz
viz =
    { kind = "julia"
    , name = "Julia set"
    , description = "Escape-time fractal of z ↦ z² + c — move c and watch it morph."
    , starter = toSource rabbit
    , movable = False
    , render = \_ source -> Result.map view (decode source)
    , controls = controls
    }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map5 (\cRe cIm maxIter resolution hue -> ( cRe, cIm, ( maxIter, resolution, hue ) ))
                    (Value.numField "cRe" fs)
                    (Value.numField "cIm" fs)
                    (Value.intField "maxIter" fs)
                    (Value.intField "resolution" fs)
                    (Value.numField "hue" fs)
                    |> Result.andThen
                        (\( cRe, cIm, ( maxIter, resolution, hue ) ) ->
                            Result.map (\stroke -> Model cRe cIm maxIter resolution hue stroke)
                                (Value.strField "stroke" fs)
                        )
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"julia\"\n"
        ++ "    , cRe = "
        ++ Value.numStr d.cRe
        ++ "\n    , cIm = "
        ++ Value.numStr d.cIm
        ++ "\n    , maxIter = "
        ++ String.fromInt d.maxIter
        ++ "\n    , resolution = "
        ++ String.fromInt d.resolution
        ++ "\n    , hue = "
        ++ Value.numStr d.hue
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"



-- RENDER ------------------------------------------------------------------------------------------


view : Model -> Svg msg
view d =
    let
        res =
            clamp 30 160 d.resolution

        maxIter =
            clamp 10 120 d.maxIter

        extent =
            1.6

        s =
            580 / toFloat res

        w =
            Draw.r1 (s + 0.6)

        domain k =
            -extent + 2 * extent * toFloat k / toFloat (res - 1)

        rect i j =
            let
                sx =
                    -290 + toFloat i * s

                sy =
                    -290 + toFloat j * s
            in
            "M" ++ Draw.r1 sx ++ " " ++ Draw.r1 sy ++ "h" ++ w ++ "v" ++ w ++ "h-" ++ w ++ "z"

        -- (escape band, rect) for every cell
        cells =
            List.concatMap
                (\j ->
                    List.map
                        (\i -> ( escape (domain i) (domain j) d.cRe d.cIm maxIter, rect i j ))
                        (List.range 0 (res - 1))
                )
                (List.range 0 (res - 1))

        buckets =
            List.foldl
                (\( band, r ) dict -> Dict.update band (\m -> Just (r :: Maybe.withDefault [] m)) dict)
                Dict.empty
                cells

        bandPath ( band, rects ) =
            Svg.path
                [ A.d (String.concat rects)
                , A.fill (bandColor d.hue maxIter d.stroke band)
                ]
                []
    in
    Draw.stage (List.map bandPath (Dict.toList buckets))


{-| Escape time of `z₀` under `z ↦ z² + c`: the step at which |z| first exceeds 2, or `maxIter`. -}
escape : Float -> Float -> Float -> Float -> Int -> Int
escape zx0 zy0 cx cy maxIter =
    let
        go zx zy n =
            if n >= maxIter then
                maxIter

            else if zx * zx + zy * zy > 4 then
                n

            else
                go (zx * zx - zy * zy + cx) (2 * zx * zy + cy) (n + 1)
    in
    go zx0 zy0 0


{-| The colour of an escape band: the in-set colour for points that never escaped, otherwise a hue
ramp keyed off the model's base `hue`. -}
bandColor : Float -> Int -> String -> Int -> String
bandColor hue maxIter inset band =
    if band >= maxIter then
        inset

    else
        "hsl("
            ++ String.fromInt (modBy 360 (round hue + band * 9))
            ++ ", 85%, "
            ++ String.fromInt (clamp 25 70 (32 + band))
            ++ "%)"



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a Julia set yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Constant  c = cRe + cIm·i"
                    [ Form.slider "cRe" -1 1 0.001 d.cRe (\v -> toSource { d | cRe = v })
                    , Form.slider "cIm" -1 1 0.001 d.cIm (\v -> toSource { d | cIm = v })
                    ]
                , Form.group "Colour"
                    [ Form.slider "hue" 0 360 1 d.hue (\v -> toSource { d | hue = v })
                    , Form.colorRow "in-set" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Quality"
                    [ Form.slider "max iterations" 20 120 1 (toFloat d.maxIter) (\v -> toSource { d | maxIter = round v })
                    , Form.slider "resolution" 40 150 2 (toFloat d.resolution) (\v -> toSource { d | resolution = round v })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Rabbit" (toSource rabbit)
                        , Form.preset "Dendrite" (toSource { d | cRe = 0, cIm = 1 })
                        , Form.preset "San Marco" (toSource { d | cRe = -0.75, cIm = 0 })
                        , Form.preset "Spiral" (toSource { d | cRe = -0.4, cIm = 0.6 })
                        ]
                    ]
                ]



-- PRESETS -----------------------------------------------------------------------------------------


rabbit : Model
rabbit =
    { cRe = -0.123, cIm = 0.745, maxIter = 60, resolution = 100, hue = 200, stroke = "#0b0e14" }
