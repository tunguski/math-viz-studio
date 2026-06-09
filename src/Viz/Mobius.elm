module Viz.Mobius exposing (viz)

{-| **Möbius strip** — a band with a half-twist, the classic one-sided surface: trace a finger along
it and you cover both "faces" without crossing an edge. Drawn as a rotating wireframe swept around
the parameter `u ∈ [0, 2π]` with a single half-twist `cos(u/2)`.
-}

import Color
import Draw exposing (Vec3)
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Value
import Viz exposing (Viz)


type alias Model =
    { width : Float
    , segU : Int
    , segV : Int
    , yaw : Float
    , pitch : Float
    , stroke : String
    }


viz : Viz
viz =
    { kind = "mobius"
    , name = "Möbius strip"
    , description = "A band with a half-twist — the classic one-sided surface."
    , about = "Discovered independently by August Möbius and Johann Listing in 1858, the Möbius strip is the simplest surface that is non-orientable: it has only one side and one edge. Cut along its centre line and it does not fall apart — you get one longer, twice-twisted loop.\n\nIt became an emblem of topology, the branch of mathematics concerned with properties that survive bending and stretching. Beyond the maths it turns up in conveyor belts and recording tapes (to wear both sides evenly), in the recycling symbol, and as a favourite of M. C. Escher."
    , starter = toSource default
    , movable = True
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { width = 0.5, segU = 44, segV = 7, yaw = 0.6, pitch = 1.1, stroke = "#fcd34d" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map5 (\width segU segV yaw pitch -> ( width, segU, ( segV, yaw, pitch ) ))
                    (Value.numField "width" fs)
                    (Value.intField "segU" fs)
                    (Value.intField "segV" fs)
                    (Value.numField "yaw" fs)
                    (Value.numField "pitch" fs)
                    |> Result.andThen
                        (\( width, segU, ( segV, yaw, pitch ) ) ->
                            Result.map (\stroke -> Model width segU segV yaw pitch stroke)
                                (Value.strField "stroke" fs)
                        )
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"mobius\"\n    , width = "
        ++ Value.numStr d.width
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
            clamp 12 64 d.segU

        vs =
            clamp 3 16 d.segV

        hw =
            d.width / 2

        point ui vj =
            let
                u =
                    2 * pi * toFloat ui / toFloat (us - 1)

                v =
                    -1 + 2 * toFloat vj / toFloat (vs - 1)

                rr =
                    1 + v * hw * cos (u / 2)
            in
            { x = rr * cos u, y = rr * sin u, z = v * hw * sin (u / 2) }

        grid =
            List.concatMap (\ui -> List.map (point ui) (List.range 0 (vs - 1))) (List.range 0 (us - 1))
    in
    \mode phase -> Draw.surface (d.yaw + phase) d.pitch us vs False False grid (Color.solid (Color.resolve mode d.stroke) phase)



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a Möbius strip yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Strip"
                    [ Form.slider "width" 0.2 1 0.02 d.width (\v -> toSource { d | width = v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Mesh"
                    [ Form.slider "length divisions" 16 64 1 (toFloat d.segU) (\v -> toSource { d | segU = round v })
                    , Form.slider "width divisions" 3 16 1 (toFloat d.segV) (\v -> toSource { d | segV = round v })
                    , Form.slider "pitch" 0 6.2832 0.01 d.pitch (\v -> toSource { d | pitch = v })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Classic" (toSource { d | width = 0.5 })
                        , Form.preset "Wide" (toSource { d | width = 0.85 })
                        , Form.preset "Narrow" (toSource { d | width = 0.3 })
                        , Form.preset "Fine mesh" (toSource { d | segU = 60, segV = 11 })
                        , Form.preset "Edge view" (toSource { d | pitch = 1.5708 })
                        , Form.preset "Flat" (toSource { d | pitch = 0.2 })
                        ]
                    ]
                ]
