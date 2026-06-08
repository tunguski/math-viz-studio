module Viz.Hilbert exposing (viz)

{-| **Hilbert curve** — a space-filling curve: a single line that, as its `order` rises, visits every
cell of a 2ⁿ × 2ⁿ grid while keeping nearby points on the line nearby in the plane. One continuous
path, so a `gradient` colouring traces the order of the visit beautifully.
-}

import Color
import Draw
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Value
import Viz exposing (Viz)


type alias Model =
    { order : Int
    , stroke : String
    }


viz : Viz
viz =
    { kind = "hilbert"
    , name = "Hilbert curve"
    , description = "A space-filling curve — one line that fills the plane."
    , starter = toSource default
    , movable = False
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { order = 5, stroke = "#7dd3fc" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map2 Model
                    (Value.intField "order" fs)
                    (Value.strField "stroke" fs)
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"hilbert\"\n"
        ++ "    , order = "
        ++ String.fromInt d.order
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"



-- RENDER ------------------------------------------------------------------------------------------


prepare : Model -> (String -> Float -> Svg msg)
prepare d =
    let
        order =
            clamp 1 6 d.order

        n =
            2 ^ order

        step =
            560 / toFloat (n - 1)

        toScreen ( x, y ) =
            ( -280 + toFloat x * step, -280 + toFloat y * step )

        pts =
            List.map (\dd -> toScreen (d2xy n dd)) (List.range 0 (n * n - 1))
    in
    \mode phase -> Draw.stage [ Draw.curve "1.4" (Color.resolve mode d.stroke) phase pts ]


{-| The Hilbert distance-to-(x, y) mapping for an n × n grid (n a power of two). -}
d2xy : Int -> Int -> ( Int, Int )
d2xy n d =
    let
        go s t x y =
            if s >= n then
                ( x, y )

            else
                let
                    rx =
                        modBy 2 (t // 2)

                    ry =
                        if modBy 2 t == rx then
                            0

                        else
                            1

                    ( x1, y1 ) =
                        if ry == 0 then
                            let
                                ( fx, fy ) =
                                    if rx == 1 then
                                        ( s - 1 - x, s - 1 - y )

                                    else
                                        ( x, y )
                            in
                            ( fy, fx )

                        else
                            ( x, y )
                in
                go (s * 2) (t // 4) (x1 + s * rx) (y1 + s * ry)
    in
    go 1 d 0 0



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a Hilbert curve yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Curve"
                    [ Form.slider "order" 1 6 1 (toFloat d.order) (\v -> toSource { d | order = round v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Order 2" (toSource { d | order = 2 })
                        , Form.preset "Order 3" (toSource { d | order = 3 })
                        , Form.preset "Order 4" (toSource { d | order = 4 })
                        , Form.preset "Order 5" (toSource { d | order = 5 })
                        , Form.preset "Order 6" (toSource { d | order = 6 })
                        ]
                    ]
                , Form.hint "Try the Gradient colour mode — it traces the order of the visit."
                ]
