module Viz.TorusKnot exposing (viz)

{-| **Torus knot** — the `(p, q)` knot that winds `p` times around a torus's axis and `q` times
through its hole, a space curve from knot theory. It is traced in 3-D, then rotated and projected (the
yaw spins with the animation clock). `(2, 3)` is the trefoil. Self-contained: model, render and
controls live here.
-}

import Draw exposing (Vec3)
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as A
import Value
import Viz exposing (Viz)


type alias Model =
    { p : Int
    , q : Int
    , samples : Int
    , yaw : Float
    , pitch : Float
    , stroke : String
    }


viz : Viz
viz =
    { kind = "knot"
    , name = "Torus knot"
    , description = "A (p, q) knot wound on a torus — the (2, 3) is a trefoil."
    , starter = toSource default
    , movable = True
    , render = \phase source -> Result.map (view phase) (decode source)
    , controls = controls
    }


default : Model
default =
    { p = 2, q = 3, samples = 900, yaw = 0.5, pitch = 0.5, stroke = "#fca5f1" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map5 (\p q samples yaw pitch -> ( p, q, ( samples, yaw, pitch ) ))
                    (Value.intField "p" fs)
                    (Value.intField "q" fs)
                    (Value.intField "samples" fs)
                    (Value.numField "yaw" fs)
                    (Value.numField "pitch" fs)
                    |> Result.andThen
                        (\( p, q, ( samples, yaw, pitch ) ) ->
                            Result.map (\stroke -> Model p q samples yaw pitch stroke)
                                (Value.strField "stroke" fs)
                        )
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"knot\"\n"
        ++ "    , p = "
        ++ String.fromInt d.p
        ++ "\n    , q = "
        ++ String.fromInt d.q
        ++ "\n    , samples = "
        ++ String.fromInt d.samples
        ++ "\n    , yaw = "
        ++ Value.numStr d.yaw
        ++ "\n    , pitch = "
        ++ Value.numStr d.pitch
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"



-- RENDER ------------------------------------------------------------------------------------------


view : Float -> Model -> Svg msg
view phase d =
    let
        path3 =
            curve d

        c =
            Draw.centroid path3

        proj =
            List.map (\v -> Draw.rotate2 (d.yaw + phase) d.pitch { x = v.x - c.x, y = v.y - c.y, z = v.z - c.z }) path3

        ( cx, cy, scale ) =
            Draw.fitTransform proj

        screen ( x, y ) =
            Draw.r2 ((x - cx) * scale) ++ "," ++ Draw.r2 (-(y - cy) * scale)

        pts =
            String.join " " (List.map screen proj)
    in
    Draw.stage
        [ Svg.polyline
            [ A.points pts
            , A.fill "none"
            , A.stroke d.stroke
            , A.strokeWidth "2"
            , A.strokeLinejoin "round"
            , A.strokeLinecap "round"
            , A.opacity "0.92"
            ]
            []
        ]


{-| The (p, q) torus knot as a closed space curve. -}
curve : Model -> List Vec3
curve d =
    let
        samples =
            clamp 50 4000 d.samples

        point i =
            let
                t =
                    2 * pi * toFloat i / toFloat samples

                r =
                    cos (toFloat d.q * t) + 2
            in
            { x = r * cos (toFloat d.p * t)
            , y = r * sin (toFloat d.p * t)
            , z = -sin (toFloat d.q * t)
            }
    in
    List.map point (List.range 0 samples)



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a torus knot yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Knot"
                    [ Form.slider "p (around)" 1 9 1 (toFloat d.p) (\v -> toSource { d | p = round v })
                    , Form.slider "q (through)" 1 9 1 (toFloat d.q) (\v -> toSource { d | q = round v })
                    , Form.slider "samples" 200 2000 50 (toFloat d.samples) (\v -> toSource { d | samples = round v })
                    ]
                , Form.group "View"
                    [ Form.slider "pitch" 0 6.2832 0.01 d.pitch (\v -> toSource { d | pitch = v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Trefoil 2·3" (toSource { d | p = 2, q = 3 })
                        , Form.preset "3·2" (toSource { d | p = 3, q = 2 })
                        , Form.preset "2·5" (toSource { d | p = 2, q = 5 })
                        , Form.preset "3·4" (toSource { d | p = 3, q = 4 })
                        ]
                    ]
                , Form.hint "The knot spins while it animates (yaw is the clock)."
                ]
