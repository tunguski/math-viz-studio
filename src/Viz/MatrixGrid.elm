module Viz.MatrixGrid exposing (viz)

{-| **Matrix transform** — how a 2×2 matrix `A = [[a, b], [c, d]]` deforms the plane. A unit grid is
mapped through `A` (rows shear, columns rotate, the whole thing scales), and the images of the basis
vectors **î** and **ĵ** — the columns of `A` — are drawn on top. The grid stays a grid of straight,
evenly-spaced lines because the map is linear.
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
    { a : Float
    , b : Float
    , c : Float
    , d : Float
    , lines : Int
    , stroke : String
    }


viz : Viz
viz =
    { kind = "matrix"
    , name = "Matrix transform"
    , description = "How a 2×2 matrix shears, rotates and scales the plane."
    , about = "A 2×2 matrix is a linear transformation of the plane: it sends each point (x, y) to a new point, keeping the origin fixed and straight, evenly-spaced lines straight and evenly spaced. The two columns of the matrix are simply where the basis vectors î = (1,0) and ĵ = (0,1) land — so reading the matrix tells you the whole map.\n\nSeeing a matrix as a moving grid, rather than a block of numbers, is the central idea of linear algebra: rotations, shears, scalings and reflections are all just matrices, the determinant is the factor by which areas grow, and composing transformations is multiplying matrices."
    , starter = toSource shear
    , movable = False
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map5 (\a b c d lines -> ( a, b, ( c, d, lines ) ))
                    (Value.numField "a" fs)
                    (Value.numField "b" fs)
                    (Value.numField "c" fs)
                    (Value.numField "d" fs)
                    (Value.intField "lines" fs)
                    |> Result.andThen
                        (\( a, b, ( c, d, lines ) ) ->
                            Result.map (\stroke -> Model a b c d lines stroke)
                                (Value.strField "stroke" fs)
                        )
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"matrix\"\n"
        ++ "    , a = "
        ++ Value.numStr d.a
        ++ "\n    , b = "
        ++ Value.numStr d.b
        ++ "\n    , c = "
        ++ Value.numStr d.c
        ++ "\n    , d = "
        ++ Value.numStr d.d
        ++ "\n    , lines = "
        ++ String.fromInt d.lines
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"



-- RENDER ------------------------------------------------------------------------------------------


prepare : Model -> (String -> Float -> Svg msg)
prepare d =
    let
        n =
            clamp 3 21 d.lines

        scale =
            130

        screen ( x, y ) =
            ( (d.a * x + d.b * y) * scale, -(d.c * x + d.d * y) * scale )

        seg p q =
            let
                ( x1, y1 ) =
                    screen p

                ( x2, y2 ) =
                    screen q
            in
            "M" ++ Draw.r1 x1 ++ " " ++ Draw.r1 y1 ++ "L" ++ Draw.r1 x2 ++ " " ++ Draw.r1 y2

        coord k =
            -1 + 2 * toFloat k / toFloat (n - 1)

        gridPath =
            String.concat
                (List.map (\k -> seg ( coord k, -1 ) ( coord k, 1 )) (List.range 0 (n - 1))
                    ++ List.map (\k -> seg ( -1, coord k ) ( 1, coord k )) (List.range 0 (n - 1))
                )
    in
    \mode phase ->
        Draw.stage
            [ Svg.path [ A.d gridPath, A.fill "none", A.stroke (Color.solid (Color.resolve mode d.stroke) phase), A.strokeWidth "1", A.opacity "0.5" ] []
            , vector ( 0, 0 ) (screen ( 1, 0 )) "#ff7b7b"
            , vector ( 0, 0 ) (screen ( 0, 1 )) "#7cfc9b"
            ]


vector : ( Float, Float ) -> ( Float, Float ) -> String -> Svg msg
vector ( x1, y1 ) ( x2, y2 ) color =
    Svg.line
        [ A.x1 (Draw.r1 x1), A.y1 (Draw.r1 y1), A.x2 (Draw.r1 x2), A.y2 (Draw.r1 y2)
        , A.stroke color, A.strokeWidth "3", A.strokeLinecap "round"
        ]
        []



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a matrix transform yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Matrix A = [[a, b], [c, d]]"
                    [ Form.slider "a" -2 2 0.05 d.a (\v -> toSource { d | a = v })
                    , Form.slider "b" -2 2 0.05 d.b (\v -> toSource { d | b = v })
                    , Form.slider "c" -2 2 0.05 d.c (\v -> toSource { d | c = v })
                    , Form.slider "d" -2 2 0.05 d.d (\v -> toSource { d | d = v })
                    , Form.colorRow "grid" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Identity" (toSource identity_)
                        , Form.preset "Rotate 45°" (toSource rotate)
                        , Form.preset "Shear" (toSource shear)
                        , Form.preset "Scale" (toSource scale_)
                        , Form.preset "Reflect" (toSource reflect)
                        , Form.preset "Squeeze" (toSource squeeze)
                        ]
                    ]
                , Form.hint "Red is î (first column of A), green is ĵ (second column)."
                ]



-- PRESETS -----------------------------------------------------------------------------------------


identity_ : Model
identity_ =
    { a = 1, b = 0, c = 0, d = 1, lines = 11, stroke = "#5fd0ff" }


rotate : Model
rotate =
    { a = 0.707, b = -0.707, c = 0.707, d = 0.707, lines = 11, stroke = "#a78bfa" }


shear : Model
shear =
    { a = 1, b = 0.6, c = 0, d = 1, lines = 11, stroke = "#fcd34d" }


scale_ : Model
scale_ =
    { a = 1.6, b = 0, c = 0, d = 0.6, lines = 11, stroke = "#7cfc9b" }


reflect : Model
reflect =
    { a = 1, b = 0, c = 0, d = -1, lines = 11, stroke = "#ff9cee" }


squeeze : Model
squeeze =
    { a = 1.5, b = 0.4, c = 0.4, d = 1.5, lines = 11, stroke = "#67e8f9" }
