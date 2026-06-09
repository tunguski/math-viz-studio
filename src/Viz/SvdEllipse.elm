module Viz.SvdEllipse exposing (viz)

{-| **Singular values** — a 2×2 matrix `A` maps the unit circle to an ellipse. The ellipse's two
half-axes are the *singular values* σ₁ ≥ σ₂ of `A`, along perpendicular directions (the left singular
vectors). This is the geometry behind the SVD: every linear map is a rotation, an axis-aligned
stretch, and another rotation.
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
    { kind = "svd"
    , name = "Singular values"
    , description = "A matrix maps the unit circle to an ellipse."
    , about = "Apply any 2×2 matrix to the unit circle and you always get an ellipse. The lengths of its two half-axes are the matrix's singular values σ₁ and σ₂, and the directions are its singular vectors — drawn here as the red (major) and green (minor) axes over the faint original circle.\n\nThis is the picture behind the singular value decomposition A = UΣVᵀ: every linear map, however tangled, is just a rotation, then a stretch along perpendicular axes, then another rotation. The SVD is one of the most useful tools in all of applied mathematics — behind data compression, principal component analysis, recommender systems and the numerical rank of a matrix."
    , starter = toSource default
    , movable = False
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { a = 1.2, b = 0.8, c = 0.3, d = 1.1, stroke = "#5fd0ff" }



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
        ++ "    { kind = \"svd\"\n    , a = "
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

        -- AᵀA = [[p,q],[q,r]]; its eigenvectors are the right singular vectors
        p =
            d.a * d.a + d.c * d.c

        q =
            d.a * d.b + d.c * d.d

        r =
            d.b * d.b + d.d * d.d

        disc =
            sqrt (((p - r) / 2) ^ 2 + q * q)

        s1 =
            sqrt (max 0 ((p + r) / 2 + disc))

        s2 =
            sqrt (max 0 ((p + r) / 2 - disc))

        v1 =
            normalize ( (p + r) / 2 + disc - r, q )

        u1 =
            normalize (applyA v1)

        u2 =
            normalize (applyA ( -(Tuple.second v1), Tuple.first v1 ))
    in
    \mode phase ->
        Draw.stage
            [ polyline unitStr "#3a4a63" "1"
            , polyline ellipseStr (Color.solid (Color.resolve mode d.stroke) phase) "2"
            , axis u1 (s1 * scale) "#ff7b7b"
            , axis u2 (s2 * scale) "#7cfc9b"
            ]


polyline : String -> String -> String -> Svg msg
polyline pts color width =
    Svg.polyline [ A.points pts, A.fill "none", A.stroke color, A.strokeWidth width, A.opacity "0.9" ] []


axis : ( Float, Float ) -> Float -> String -> Svg msg
axis ( ux, uy ) len color =
    Svg.line
        [ A.x1 (Draw.r1 (-ux * len)), A.y1 (Draw.r1 (uy * len)), A.x2 (Draw.r1 (ux * len)), A.y2 (Draw.r1 (-uy * len))
        , A.stroke color, A.strokeWidth "2.5", A.strokeLinecap "round"
        ]
        []


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
            Form.note ("This file isn't a singular-value picture yet — " ++ e)

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
                        [ Form.preset "Circle" (toSource { d | a = 1, b = 0, c = 0, d = 1 })
                        , Form.preset "Stretch" (toSource { d | a = 1.7, b = 0, c = 0, d = 0.6 })
                        , Form.preset "Shear" (toSource { d | a = 1, b = 0.9, c = 0, d = 1 })
                        , Form.preset "Rotate+stretch" (toSource { d | a = 1.2, b = 0.8, c = 0.3, d = 1.1 })
                        , Form.preset "Near-singular" (toSource { d | a = 1, b = 1, c = 0.9, d = 0.9 })
                        , Form.preset "Reflect" (toSource { d | a = 1.3, b = 0.4, c = 0.4, d = -1.2 })
                        ]
                    ]
                , Form.hint "Red and green are the major and minor axes (the singular values)."
                ]
