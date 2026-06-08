module Viz.Clifford exposing (viz)

{-| **Clifford attractor** — the orbit of the map `x' = sin(a·y) + c·cos(a·x)`,
`y' = sin(b·x) + d·cos(b·y)`. Four parameters trace a delicate, wing-like point cloud. The whole
cloud is one `<path>` of dots.
-}

import Color
import Draw
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Value
import Viz exposing (Viz)


type alias Model =
    { a : Float
    , b : Float
    , c : Float
    , d : Float
    , points : Int
    , stroke : String
    }


viz : Viz
viz =
    { kind = "clifford"
    , name = "Clifford attractor"
    , description = "A delicate, wing-like strange attractor from a 2-D map."
    , about = "This is one of the \"strange attractors\" popularised by Clifford Pickover in the 1980s and 90s. Iterating the two-line map x′ = sin(a·y) + c·cos(a·x), y′ = sin(b·x) + d·cos(b·y) sends a point wandering forever without ever repeating, yet it stays confined to an intricate, wing-like cloud.\n\nWith only four numbers it produces an endless gallery of filaments and folds — a vivid reminder that elaborate structure can emerge from a trivially short recurrence, and a favourite of generative artists."
    , starter = toSource default
    , movable = False
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { a = -1.4, b = 1.6, c = 1.0, d = 0.7, points = 40000, stroke = "#f0abfc" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map5 (\a b c d points -> ( a, b, ( c, d, points ) ))
                    (Value.numField "a" fs)
                    (Value.numField "b" fs)
                    (Value.numField "c" fs)
                    (Value.numField "d" fs)
                    (Value.intField "points" fs)
                    |> Result.andThen
                        (\( a, b, ( c, d, points ) ) ->
                            Result.map (\stroke -> Model a b c d points stroke)
                                (Value.strField "stroke" fs)
                        )
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"clifford\"\n"
        ++ "    , a = "
        ++ Value.numStr d.a
        ++ "\n    , b = "
        ++ Value.numStr d.b
        ++ "\n    , c = "
        ++ Value.numStr d.c
        ++ "\n    , d = "
        ++ Value.numStr d.d
        ++ "\n    , points = "
        ++ String.fromInt d.points
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"



-- RENDER ------------------------------------------------------------------------------------------


prepare : Model -> (String -> Float -> Svg msg)
prepare d =
    let
        raw =
            orbit d (clamp 1000 80000 d.points)

        ( cx, cy, scale ) =
            Draw.fitTransform raw

        pts =
            List.map (\( x, y ) -> ( (x - cx) * scale, -(y - cy) * scale )) raw
    in
    \mode phase -> Draw.stage [ Draw.cloud "0.7" (Color.resolve mode d.stroke) phase pts ]


orbit : Model -> Int -> List ( Float, Float )
orbit d count =
    let
        step _ ( x, y, acc ) =
            let
                nx =
                    sin (d.a * y) + d.c * cos (d.a * x)

                ny =
                    sin (d.b * x) + d.d * cos (d.b * y)
            in
            ( nx, ny, ( nx, ny ) :: acc )

        ( _, _, pts ) =
            List.foldl step ( 0.1, 0.1, [] ) (List.range 1 count)
    in
    List.drop 10 (List.reverse pts)



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a Clifford attractor yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Parameters"
                    [ Form.slider "a" -3 3 0.01 d.a (\v -> toSource { d | a = v })
                    , Form.slider "b" -3 3 0.01 d.b (\v -> toSource { d | b = v })
                    , Form.slider "c" -3 3 0.01 d.c (\v -> toSource { d | c = v })
                    , Form.slider "d" -3 3 0.01 d.d (\v -> toSource { d | d = v })
                    ]
                , Form.group "Cloud"
                    [ Form.slider "points" 5000 80000 1000 (toFloat d.points) (\v -> toSource { d | points = round v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Wings" (toSource { d | a = -1.4, b = 1.6, c = 1.0, d = 0.7 })
                        , Form.preset "Ribbon" (toSource { d | a = 1.7, b = 1.7, c = 0.6, d = 1.2 })
                        , Form.preset "Web" (toSource { d | a = -1.7, b = 1.3, c = -0.1, d = -1.2 })
                        , Form.preset "Feather" (toSource { d | a = -1.8, b = -2.0, c = -0.5, d = -0.9 })
                        , Form.preset "Swirl" (toSource { d | a = 1.5, b = -1.8, c = 1.6, d = 0.9 })
                        , Form.preset "Lace" (toSource { d | a = -1.7, b = 1.8, c = -1.9, d = -0.4 })
                        ]
                    ]
                ]
