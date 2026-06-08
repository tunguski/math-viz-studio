module Viz.Mandelbrot exposing (viz)

{-| **Mandelbrot set** — for each point `c` of the plane, how fast the orbit of `z ↦ z² + c` (from
`z = 0`) escapes. The most famous image in mathematics. You "build" it by panning (`centerX`/`centerY`)
and zooming. Like the Julia set it is drawn one filled `<path>` per escape band, so the grid stays a
few dozen DOM nodes.
-}

import Color
import Dict exposing (Dict)
import Draw
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as A
import Value
import Viz exposing (Viz)


type alias Model =
    { centerX : Float
    , centerY : Float
    , zoom : Float
    , maxIter : Int
    , resolution : Int
    , hue : Float
    , stroke : String
    }


viz : Viz
viz =
    { kind = "mandelbrot"
    , name = "Mandelbrot set"
    , description = "The most famous fractal — pan and zoom into z ↦ z² + c."
    , starter = toSource default
    , movable = False
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { centerX = -0.6, centerY = 0, zoom = 1, maxIter = 90, resolution = 110, hue = 20, stroke = "#0b0e14" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map5 (\cx cy zoom mi res -> ( cx, cy, ( zoom, mi, res ) ))
                    (Value.numField "centerX" fs)
                    (Value.numField "centerY" fs)
                    (Value.numField "zoom" fs)
                    (Value.intField "maxIter" fs)
                    (Value.intField "resolution" fs)
                    |> Result.andThen
                        (\( cx, cy, ( zoom, mi, res ) ) ->
                            Result.map2 (\hue stroke -> Model cx cy zoom mi res hue stroke)
                                (Value.numField "hue" fs)
                                (Value.strField "stroke" fs)
                        )
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"mandelbrot\"\n"
        ++ "    , centerX = "
        ++ Value.numStr d.centerX
        ++ "\n    , centerY = "
        ++ Value.numStr d.centerY
        ++ "\n    , zoom = "
        ++ Value.numStr d.zoom
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


prepare : Model -> (String -> Float -> Svg msg)
prepare d =
    let
        res =
            clamp 30 160 d.resolution

        maxIter =
            clamp 10 200 d.maxIter

        half =
            1.5 / max 0.01 d.zoom

        s =
            580 / toFloat res

        w =
            Draw.r1 (s + 0.6)

        coord c =
            -1 + 2 * toFloat c / toFloat (res - 1)

        rect i j =
            "M" ++ Draw.r1 (-290 + toFloat i * s) ++ " " ++ Draw.r1 (-290 + toFloat j * s) ++ "h" ++ w ++ "v" ++ w ++ "h-" ++ w ++ "z"

        cells =
            List.concatMap
                (\j ->
                    List.map
                        (\i ->
                            ( escape (d.centerX + coord i * half) (d.centerY + coord j * half) maxIter, rect i j )
                        )
                        (List.range 0 (res - 1))
                )
                (List.range 0 (res - 1))

        buckets =
            List.foldl
                (\( band, r ) dict -> Dict.update band (\m -> Just (r :: Maybe.withDefault [] m)) dict)
                Dict.empty
                cells
    in
    \mode phase ->
        let
            offset =
                if Color.timeVarying mode then
                    phase * 60

                else
                    0

            bandPath ( band, rects ) =
                Svg.path [ A.d (String.concat rects), A.fill (bandColor (d.hue + offset) maxIter d.stroke band) ] []
        in
        Draw.stage (List.map bandPath (Dict.toList buckets))


{-| Escape time of `c` under `z ↦ z² + c` from `z = 0`. -}
escape : Float -> Float -> Int -> Int
escape cx cy maxIter =
    let
        go zx zy n =
            if n >= maxIter then
                maxIter

            else if zx * zx + zy * zy > 4 then
                n

            else
                go (zx * zx - zy * zy + cx) (2 * zx * zy + cy) (n + 1)
    in
    go 0 0 0


bandColor : Float -> Int -> String -> Int -> String
bandColor hue maxIter inset band =
    if band >= maxIter then
        inset

    else
        "hsl(" ++ String.fromInt (modBy 360 (round hue + band * 8)) ++ ", 80%, " ++ String.fromInt (clamp 25 70 (32 + band)) ++ "%)"



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a Mandelbrot set yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Viewport"
                    [ Form.slider "centre x" -2 1 0.001 d.centerX (\v -> toSource { d | centerX = v })
                    , Form.slider "centre y" -1.5 1.5 0.001 d.centerY (\v -> toSource { d | centerY = v })
                    , Form.slider "zoom" 0.5 200 0.5 d.zoom (\v -> toSource { d | zoom = v })
                    ]
                , Form.group "Colour"
                    [ Form.slider "hue" 0 360 1 d.hue (\v -> toSource { d | hue = v })
                    , Form.colorRow "in-set" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Quality"
                    [ Form.slider "max iterations" 20 200 1 (toFloat d.maxIter) (\v -> toSource { d | maxIter = round v })
                    , Form.slider "resolution" 40 150 2 (toFloat d.resolution) (\v -> toSource { d | resolution = round v })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Full" (toSource { d | centerX = -0.6, centerY = 0, zoom = 1 })
                        , Form.preset "Seahorse" (toSource { d | centerX = -0.745, centerY = 0.105, zoom = 30 })
                        , Form.preset "Spiral" (toSource { d | centerX = -0.7269, centerY = 0.1889, zoom = 90 })
                        , Form.preset "Mini-brot" (toSource { d | centerX = -1.749, centerY = 0, zoom = 70 })
                        , Form.preset "Tendrils" (toSource { d | centerX = -0.235, centerY = 0.827, zoom = 55 })
                        , Form.preset "Valley" (toSource { d | centerX = -0.16, centerY = 1.035, zoom = 45 })
                        ]
                    ]
                ]
