module Viz.VectorField exposing (viz)

{-| **Vector field** — the phase portrait of the linear system `(x, y)' = A·(x, y)` for a 2×2 matrix
`A = [[a, b], [c, d]]`. An arrow at each grid point shows the flow direction; the matrix's eigenvalues
decide whether it is a node, saddle, spiral or centre. Self-contained: model, decode/print, render
and controls all live here.
-}

import Draw
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as A
import Value
import Viz exposing (Viz)


type alias Model =
    { a : Float
    , b : Float
    , c : Float
    , d : Float
    , grid : Int
    , stroke : String
    }


viz : Viz
viz =
    { kind = "field"
    , name = "Vector field"
    , description = "The phase portrait of a 2×2 linear system."
    , starter = toSource spiral
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
                Result.map5 (\a b c d grid -> ( a, b, ( c, d, grid ) ))
                    (Value.numField "a" fs)
                    (Value.numField "b" fs)
                    (Value.numField "c" fs)
                    (Value.numField "d" fs)
                    (Value.intField "grid" fs)
                    |> Result.andThen
                        (\( a, b, ( c, d, grid ) ) ->
                            Result.map (\stroke -> Model a b c d grid stroke)
                                (Value.strField "stroke" fs)
                        )
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"field\"\n"
        ++ "    , a = "
        ++ Value.numStr d.a
        ++ "\n    , b = "
        ++ Value.numStr d.b
        ++ "\n    , c = "
        ++ Value.numStr d.c
        ++ "\n    , d = "
        ++ Value.numStr d.d
        ++ "\n    , grid = "
        ++ String.fromInt d.grid
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"



-- RENDER ------------------------------------------------------------------------------------------


view : Model -> Svg msg
view d =
    let
        g =
            clamp 4 28 d.grid

        extent =
            260.0

        cell =
            2 * extent / toFloat g

        -- one arrow at each grid point, all concatenated into a single path
        arrow i j =
            let
                px =
                    -extent + cell * (toFloat i + 0.5)

                py =
                    -extent + cell * (toFloat j + 0.5)

                -- the field is evaluated in [-1, 1] space; screen y is down, so flip it
                x =
                    px / extent

                y =
                    -py / extent

                vx =
                    d.a * x + d.b * y

                vy =
                    d.c * x + d.d * y

                mag =
                    sqrt (vx * vx + vy * vy) + 0.0001

                len =
                    cell * 0.42

                hx =
                    px + vx / mag * len

                -- screen y is down, so the field's +y points up the screen
                hy =
                    py - vy / mag * len

                -- arrowhead: two barbs from the head, the back direction rotated by ±26°
                bx =
                    -(hx - px) / len

                by =
                    -(hy - py) / len

                bl =
                    len * 0.45

                seg x1 y1 x2 y2 =
                    "M" ++ Draw.r1 x1 ++ " " ++ Draw.r1 y1 ++ "L" ++ Draw.r1 x2 ++ " " ++ Draw.r1 y2
            in
            seg px py hx hy
                ++ seg hx hy (hx + bl * (bx * 0.9 - by * 0.44)) (hy + bl * (bx * 0.44 + by * 0.9))
                ++ seg hx hy (hx + bl * (bx * 0.9 + by * 0.44)) (hy + bl * (-bx * 0.44 + by * 0.9))

        paths =
            String.concat
                (List.concatMap
                    (\i -> List.map (\j -> arrow i j) (List.range 0 (g - 1)))
                    (List.range 0 (g - 1))
                )
    in
    Draw.stage
        [ Svg.path
            [ A.d paths
            , A.fill "none"
            , A.stroke d.stroke
            , A.strokeWidth "1.4"
            , A.strokeLinecap "round"
            , A.strokeLinejoin "round"
            , A.opacity "0.85"
            ]
            []
        ]



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a vector field yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Matrix A = [[a, b], [c, d]]"
                    [ Form.slider "a" -2 2 0.05 d.a (\v -> toSource { d | a = v })
                    , Form.slider "b" -2 2 0.05 d.b (\v -> toSource { d | b = v })
                    , Form.slider "c" -2 2 0.05 d.c (\v -> toSource { d | c = v })
                    , Form.slider "d" -2 2 0.05 d.d (\v -> toSource { d | d = v })
                    ]
                , Form.group "Field"
                    [ Form.slider "grid" 6 24 1 (toFloat d.grid) (\v -> toSource { d | grid = round v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Spiral" (toSource spiral)
                        , Form.preset "Rotation" (toSource rotation)
                        , Form.preset "Saddle" (toSource saddle)
                        , Form.preset "Node" (toSource node)
                        ]
                    ]
                ]



-- PRESETS -----------------------------------------------------------------------------------------


spiral : Model
spiral =
    { a = -0.3, b = -1, c = 1, d = -0.3, grid = 15, stroke = "#a78bfa" }


rotation : Model
rotation =
    { a = 0, b = -1, c = 1, d = 0, grid = 15, stroke = "#5fd0ff" }


saddle : Model
saddle =
    { a = 1, b = 0, c = 0, d = -1, grid = 15, stroke = "#ff9cee" }


node : Model
node =
    { a = 1, b = 0, c = 0, d = 1, grid = 15, stroke = "#7cfc9b" }
