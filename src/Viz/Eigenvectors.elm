module Viz.Eigenvectors exposing (viz)

{-| **Eigenvectors** — the special directions a 2×2 matrix `A` does not turn: a vector `v` on an
eigenline maps to a scalar multiple `λv`, staying on the same line. Shown over the unit circle and
its image ellipse, the bold red/green lines are the (real) eigen-directions.
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
    , stroke : String
    }


viz : Viz
viz =
    { kind = "eigen"
    , name = "Eigenvectors"
    , description = "The directions a matrix only stretches, never turns."
    , about = "An eigenvector of a matrix is a direction that the matrix leaves alone: it may stretch or flip a vector lying along it, by the factor λ (the eigenvalue), but never rotate it off its line. Those invariant lines are drawn here in red and green over the unit circle and the ellipse it maps to.\n\nEigenvectors are one of the deepest ideas in linear algebra. They reveal the natural axes of a transformation — the principal stresses in a beam, the standing-wave modes of a drum, the steady state of a Markov chain (Google's PageRank is an eigenvector), and the principal components of data. When a matrix has no real eigenvectors it is a rotation, and no line survives unturned."
    , starter = toSource default
    , movable = False
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { a = 1.2, b = 0.7, c = 0.4, d = 0.9, stroke = "#5fd0ff" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map5 Model
                    (Value.numField "a" fs)
                    (Value.numField "b" fs)
                    (Value.numField "c" fs)
                    (Value.numField "d" fs)
                    (Value.strField "stroke" fs)
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"eigen\"\n    , a = "
        ++ Value.numStr d.a
        ++ "\n    , b = "
        ++ Value.numStr d.b
        ++ "\n    , c = "
        ++ Value.numStr d.c
        ++ "\n    , d = "
        ++ Value.numStr d.d
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"



-- RENDER ------------------------------------------------------------------------------------------


prepare : Model -> (String -> Float -> Svg msg)
prepare d =
    let
        scale =
            105

        applyA ( x, y ) =
            ( d.a * x + d.b * y, d.c * x + d.d * y )

        circle =
            List.map (\i -> let t = 2 * pi * toFloat i / 96 in ( cos t, sin t )) (List.range 0 96)

        screenStr ( x, y ) =
            Draw.r1 (x * scale) ++ "," ++ Draw.r1 (-y * scale)

        unitStr =
            String.join " " (List.map screenStr circle)

        ellipseStr =
            String.join " " (List.map (screenStr << applyA) circle)

        tr =
            d.a + d.d

        det =
            d.a * d.d - d.b * d.c

        disc =
            (tr / 2) ^ 2 - det

        eigenLines =
            if disc >= 0 then
                let
                    rt =
                        sqrt disc
                in
                [ eigenLine (eigenvec d (tr / 2 + rt)) "#ff7b7b"
                , eigenLine (eigenvec d (tr / 2 - rt)) "#7cfc9b"
                ]

            else
                []
    in
    \mode phase ->
        Draw.stage
            (polyline unitStr "#3a4a63" "1"
                :: polyline ellipseStr (Color.solid (Color.resolve mode d.stroke) phase) "1.6"
                :: eigenLines
            )


eigenvec : Model -> Float -> ( Float, Float )
eigenvec d lam =
    if abs d.c > 1.0e-9 then
        normalize ( lam - d.d, d.c )

    else if abs d.b > 1.0e-9 then
        normalize ( d.b, lam - d.a )

    else if abs (lam - d.a) < abs (lam - d.d) then
        ( 1, 0 )

    else
        ( 0, 1 )


eigenLine : ( Float, Float ) -> String -> Svg msg
eigenLine ( ux, uy ) color =
    Svg.line
        [ A.x1 (Draw.r1 (-ux * 295)), A.y1 (Draw.r1 (uy * 295)), A.x2 (Draw.r1 (ux * 295)), A.y2 (Draw.r1 (-uy * 295))
        , A.stroke color, A.strokeWidth "2.5", A.strokeLinecap "round", A.opacity "0.95"
        ]
        []


polyline : String -> String -> String -> Svg msg
polyline pts color width =
    Svg.polyline [ A.points pts, A.fill "none", A.stroke color, A.strokeWidth width, A.opacity "0.9" ] []


normalize : ( Float, Float ) -> ( Float, Float )
normalize ( x, y ) =
    let
        m =
            sqrt (x * x + y * y)
    in
    if m < 1.0e-9 then
        ( 1, 0 )

    else
        ( x / m, y / m )



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't an eigenvector picture yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Matrix A = [[a, b], [c, d]]"
                    [ Form.slider "a" -2 2 0.05 d.a (\v -> toSource { d | a = v })
                    , Form.slider "b" -2 2 0.05 d.b (\v -> toSource { d | b = v })
                    , Form.slider "c" -2 2 0.05 d.c (\v -> toSource { d | c = v })
                    , Form.slider "d" -2 2 0.05 d.d (\v -> toSource { d | d = v })
                    , Form.colorRow "ellipse" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Two real" (toSource { d | a = 1.2, b = 0.7, c = 0.4, d = 0.9 })
                        , Form.preset "Diagonal" (toSource { d | a = 1.6, b = 0, c = 0, d = 0.6 })
                        , Form.preset "Shear" (toSource { d | a = 1, b = 0.8, c = 0, d = 1 })
                        , Form.preset "Saddle" (toSource { d | a = 0.4, b = 1.1, c = 1.1, d = 0.4 })
                        , Form.preset "Rotation (none)" (toSource { d | a = 0.2, b = -1, c = 1, d = 0.2 })
                        , Form.preset "Reflection" (toSource { d | a = 1, b = 0, c = 0, d = -1 })
                        ]
                    ]
                , Form.hint "No real eigenvectors? The matrix is rotating — every direction turns."
                ]
