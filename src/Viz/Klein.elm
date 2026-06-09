module Viz.Klein exposing (viz)

{-| **Klein bottle** — a closed, one-sided surface with no inside or outside. It can't exist without
self-intersecting in 3-D, so this is the standard "figure-8" immersion, drawn as a rotating
wireframe.
-}

import Color
import Draw exposing (Vec3)
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Value
import Viz exposing (Viz)


type alias Model =
    { bulge : Float
    , segU : Int
    , segV : Int
    , yaw : Float
    , pitch : Float
    , stroke : String
    }


viz : Viz
viz =
    { kind = "klein"
    , name = "Klein bottle"
    , description = "A closed one-sided surface — no inside or outside."
    , about = "Described by Felix Klein in 1882, the Klein bottle is what you get by gluing two Möbius strips along their edges: a closed surface with only one side, where \"inside\" and \"outside\" are the same place. A true Klein bottle lives in four dimensions and passes through itself only when squeezed into our three.\n\nIt is a centrepiece of topology and a beloved mathematical curiosity — glassblowers make Klein-bottle vessels, and it poses the playful question of how to fill a bottle that has no inside. This figure-8 immersion is one common way to draw it."
    , starter = toSource default
    , movable = True
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { bulge = 0, segU = 40, segV = 20, yaw = 0.5, pitch = 1.1, stroke = "#7cfc9b" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map5 (\bulge segU segV yaw pitch -> ( bulge, segU, ( segV, yaw, pitch ) ))
                    (Value.numField "bulge" fs)
                    (Value.intField "segU" fs)
                    (Value.intField "segV" fs)
                    (Value.numField "yaw" fs)
                    (Value.numField "pitch" fs)
                    |> Result.andThen
                        (\( bulge, segU, ( segV, yaw, pitch ) ) ->
                            Result.map (\stroke -> Model bulge segU segV yaw pitch stroke)
                                (Value.strField "stroke" fs)
                        )
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"klein\"\n    , bulge = "
        ++ Value.numStr d.bulge
        ++ "\n    , segU = "
        ++ String.fromInt d.segU
        ++ "\n    , segV = "
        ++ String.fromInt d.segV
        ++ "\n    , yaw = "
        ++ Value.numStr d.yaw
        ++ "\n    , pitch = "
        ++ Value.numStr d.pitch
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"



-- RENDER ------------------------------------------------------------------------------------------


prepare : Model -> (String -> Float -> Svg msg)
prepare d =
    let
        us =
            clamp 12 56 d.segU

        vs =
            clamp 8 40 d.segV

        r =
            2 + d.bulge

        point ui vj =
            let
                u =
                    2 * pi * toFloat ui / toFloat us

                v =
                    2 * pi * toFloat vj / toFloat vs

                rr =
                    r + cos (u / 2) * sin v - sin (u / 2) * sin (2 * v)
            in
            { x = rr * cos u, y = rr * sin u, z = sin (u / 2) * sin v + cos (u / 2) * sin (2 * v) }

        grid =
            List.concatMap (\ui -> List.map (point ui) (List.range 0 (vs - 1))) (List.range 0 (us - 1))
    in
    \mode phase -> Draw.surface (d.yaw + phase) d.pitch us vs True True grid (Color.solid (Color.resolve mode d.stroke) phase)



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a Klein bottle yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Shape"
                    [ Form.slider "bulge" -1.2 2 0.05 d.bulge (\v -> toSource { d | bulge = v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Mesh"
                    [ Form.slider "u divisions" 12 56 1 (toFloat d.segU) (\v -> toSource { d | segU = round v })
                    , Form.slider "v divisions" 8 40 1 (toFloat d.segV) (\v -> toSource { d | segV = round v })
                    , Form.slider "pitch" 0 6.2832 0.01 d.pitch (\v -> toSource { d | pitch = v })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Classic" (toSource { d | bulge = 0 })
                        , Form.preset "Pinched" (toSource { d | bulge = -0.8 })
                        , Form.preset "Wide" (toSource { d | bulge = 1.5 })
                        , Form.preset "Fine mesh" (toSource { d | segU = 52, segV = 32 })
                        , Form.preset "Top view" (toSource { d | pitch = 0.1 })
                        , Form.preset "Side view" (toSource { d | pitch = 1.5708 })
                        ]
                    ]
                ]
