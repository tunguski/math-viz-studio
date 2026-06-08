module Viz.Superformula exposing (viz)

{-| **Superformula** (Gielis) — the polar curve
`r(θ) = (|cos(mθ/4)/a|^n2 + |sin(mθ/4)/b|^n3)^(−1/n1)`. A handful of numbers sweep from circles and
polygons to flowers, stars and organic blobs. Traced as one curve, so it takes any colouring.
-}

import Color
import Draw
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Value
import Viz exposing (Viz)


type alias Model =
    { m : Float
    , n1 : Float
    , n2 : Float
    , n3 : Float
    , a : Float
    , b : Float
    , stroke : String
    }


viz : Viz
viz =
    { kind = "superformula"
    , name = "Superformula"
    , description = "Gielis's formula — circles, stars, flowers and blobs from six numbers."
    , starter = toSource default
    , movable = False
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { m = 6, n1 = 1, n2 = 1, n3 = 1, a = 1, b = 1, stroke = "#fbbf24" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map5 (\m n1 n2 n3 a -> ( m, n1, ( n2, n3, a ) ))
                    (Value.numField "m" fs)
                    (Value.numField "n1" fs)
                    (Value.numField "n2" fs)
                    (Value.numField "n3" fs)
                    (Value.numField "a" fs)
                    |> Result.andThen
                        (\( m, n1, ( n2, n3, a ) ) ->
                            Result.map2 (\b stroke -> Model m n1 n2 n3 a b stroke)
                                (Value.numField "b" fs)
                                (Value.strField "stroke" fs)
                        )
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"superformula\"\n"
        ++ "    , m = "
        ++ Value.numStr d.m
        ++ "\n    , n1 = "
        ++ Value.numStr d.n1
        ++ "\n    , n2 = "
        ++ Value.numStr d.n2
        ++ "\n    , n3 = "
        ++ Value.numStr d.n3
        ++ "\n    , a = "
        ++ Value.numStr d.a
        ++ "\n    , b = "
        ++ Value.numStr d.b
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"



-- RENDER ------------------------------------------------------------------------------------------


prepare : Model -> (String -> Float -> Svg msg)
prepare d =
    let
        samples =
            720

        point i =
            let
                theta =
                    2 * pi * toFloat i / toFloat samples

                r =
                    superformula d theta
            in
            ( r * cos theta, r * sin theta )

        raw =
            List.map point (List.range 0 samples)

        ( cx, cy, scale ) =
            Draw.fitTransform raw

        pts =
            List.map (\( x, y ) -> ( (x - cx) * scale, -(y - cy) * scale )) raw
    in
    \mode phase -> Draw.stage [ Draw.curve "1.4" (Color.resolve mode d.stroke) phase pts ]


superformula : Model -> Float -> Float
superformula d theta =
    let
        t1 =
            abs (cos (d.m * theta / 4) / d.a) ^ d.n2

        t2 =
            abs (sin (d.m * theta / 4) / d.b) ^ d.n3

        sum =
            t1 + t2
    in
    if sum <= 0 then
        0

    else
        sum ^ (-1 / d.n1)



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a superformula yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Shape"
                    [ Form.slider "m (symmetry)" 0 20 0.5 d.m (\v -> toSource { d | m = v })
                    , Form.slider "n1" 0.1 8 0.1 d.n1 (\v -> toSource { d | n1 = v })
                    , Form.slider "n2" 0.1 8 0.1 d.n2 (\v -> toSource { d | n2 = v })
                    , Form.slider "n3" 0.1 8 0.1 d.n3 (\v -> toSource { d | n3 = v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Flower" (toSource { d | m = 6, n1 = 1, n2 = 1, n3 = 1 })
                        , Form.preset "Starfish" (toSource { d | m = 5, n1 = 2, n2 = 7, n3 = 7 })
                        , Form.preset "Gear" (toSource { d | m = 12, n1 = 0.3, n2 = 0.3, n3 = 0.3 })
                        , Form.preset "Blob" (toSource { d | m = 3, n1 = 5, n2 = 18, n3 = 18 })
                        ]
                    ]
                ]
