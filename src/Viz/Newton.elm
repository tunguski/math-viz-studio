module Viz.Newton exposing (viz)

{-| **Newton fractal** — colour each point of the plane by which root of `z³ − 1` Newton's method
converges to from there, shaded by how many steps it took. The basins meet along an intricate fractal
boundary. Drawn one filled `<path>` per (root, speed) band, like the other grids.
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
    { resolution : Int
    , maxIter : Int
    , hue : Float
    , stroke : String
    }


viz : Viz
viz =
    { kind = "newton"
    , name = "Newton fractal"
    , description = "Newton's method basins for z³ − 1 — three colours, a fractal border."
    , starter = toSource default
    , movable = False
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { resolution = 120, maxIter = 28, hue = 0, stroke = "#0b0e14" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map4 Model
                    (Value.intField "resolution" fs)
                    (Value.intField "maxIter" fs)
                    (Value.numField "hue" fs)
                    (Value.strField "stroke" fs)
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"newton\"\n"
        ++ "    , resolution = "
        ++ String.fromInt d.resolution
        ++ "\n    , maxIter = "
        ++ String.fromInt d.maxIter
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
            clamp 5 80 d.maxIter

        extent =
            1.8

        s =
            580 / toFloat res

        w =
            Draw.r1 (s + 0.6)

        coord c =
            -extent + 2 * extent * toFloat c / toFloat (res - 1)

        rect i j =
            "M" ++ Draw.r1 (-290 + toFloat i * s) ++ " " ++ Draw.r1 (-290 + toFloat j * s) ++ "h" ++ w ++ "v" ++ w ++ "h-" ++ w ++ "z"

        -- band key: rootIndex * 8 + min 7 (steps // 3)
        cells =
            List.concatMap
                (\j ->
                    List.map (\i -> ( basin (coord i) (coord j) maxIter, rect i j )) (List.range 0 (res - 1))
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
                Svg.path [ A.d (String.concat rects), A.fill (bandColor (d.hue + offset) d.stroke band) ] []
        in
        Draw.stage (List.map bandPath (Dict.toList buckets))


{-| Iterate Newton's method for `z³ − 1` and return a band key combining which root it reached with
how quickly. -}
basin : Float -> Float -> Int -> Int
basin x0 y0 maxIter =
    let
        go x y n =
            if n >= maxIter then
                -1

            else
                let
                    -- f = z³ − 1, f' = 3z²; step z − f/f'  (complex)
                    x2 =
                        x * x - y * y

                    y2 =
                        2 * x * y

                    -- z³
                    fx =
                        x2 * x - y2 * y - 1

                    fy =
                        x2 * y + y2 * x

                    -- 3z²
                    dx =
                        3 * x2

                    dy =
                        3 * y2

                    den =
                        dx * dx + dy * dy + 1.0e-12

                    -- f / f'
                    qx =
                        (fx * dx + fy * dy) / den

                    qy =
                        (fy * dx - fx * dy) / den

                    nx =
                        x - qx

                    ny =
                        y - qy
                in
                if (nx - x) * (nx - x) + (ny - y) * (ny - y) < 1.0e-6 then
                    rootOf nx ny * 8 + min 7 (n // 2)

                else
                    go nx ny (n + 1)
    in
    go x0 y0 0


{-| Which cube root of unity (0, 1, 2) the point is nearest. -}
rootOf : Float -> Float -> Int
rootOf x y =
    let
        d0 =
            (x - 1) * (x - 1) + y * y

        d1 =
            (x + 0.5) * (x + 0.5) + (y - 0.8660254) * (y - 0.8660254)

        d2 =
            (x + 0.5) * (x + 0.5) + (y + 0.8660254) * (y + 0.8660254)
    in
    if d0 <= d1 && d0 <= d2 then
        0

    else if d1 <= d2 then
        1

    else
        2


bandColor : Float -> String -> Int -> String
bandColor hue inset band =
    if band < 0 then
        inset

    else
        let
            root =
                band // 8

            speed =
                modBy 8 band
        in
        "hsl(" ++ String.fromInt (modBy 360 (round hue + root * 120)) ++ ", 70%, " ++ String.fromInt (clamp 20 70 (24 + speed * 6)) ++ "%)"



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a Newton fractal yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Colour"
                    [ Form.slider "hue" 0 360 1 d.hue (\v -> toSource { d | hue = v })
                    , Form.colorRow "non-converged" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Quality"
                    [ Form.slider "max iterations" 8 80 1 (toFloat d.maxIter) (\v -> toSource { d | maxIter = round v })
                    , Form.slider "resolution" 40 150 2 (toFloat d.resolution) (\v -> toSource { d | resolution = round v })
                    ]
                ]
