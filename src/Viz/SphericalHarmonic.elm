module Viz.SphericalHarmonic exposing (viz)

{-| **Spherical harmonic** — a sphere whose radius is modulated by a harmonic pattern
`1 + bump·sin(l·v)·cos(m·u)`, drawn as a rotating wireframe. The lobes are a cousin of the real
spherical harmonics Yₗᵐ that describe atomic orbitals and the vibration modes of a sphere.
-}

import Color
import Draw exposing (Vec3)
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Value
import Viz exposing (Viz)


type alias Model =
    { l : Int
    , m : Int
    , bump : Float
    , segU : Int
    , segV : Int
    , yaw : Float
    , pitch : Float
    , stroke : String
    }


viz : Viz
viz =
    { kind = "sphere"
    , name = "Spherical harmonic"
    , description = "A sphere rippled by a harmonic pattern of lobes."
    , about = "Spherical harmonics are the natural \"notes\" of a sphere — the functions Yₗᵐ that arise whenever a problem has spherical symmetry. They describe the shapes of atomic orbitals in chemistry, the angular patterns of gravitational and magnetic fields, and the ripples mapped across the cosmic microwave background.\n\nHere a sphere's radius is pushed in and out by such a pattern, with l controlling the bands of latitude and m the lobes of longitude. Spinning the wireframe shows how two small integers carve a sphere into a surprising variety of bulging, petalled shapes."
    , starter = toSource default
    , movable = True
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { l = 4, m = 4, bump = 0.45, segU = 32, segV = 24, yaw = 0.6, pitch = 1.2, stroke = "#a78bfa" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map5 (\l m bump segU segV -> ( l, m, ( bump, segU, segV ) ))
                    (Value.intField "l" fs)
                    (Value.intField "m" fs)
                    (Value.numField "bump" fs)
                    (Value.intField "segU" fs)
                    (Value.intField "segV" fs)
                    |> Result.andThen
                        (\( l, m, ( bump, segU, segV ) ) ->
                            Result.map3 (\yaw pitch stroke -> Model l m bump segU segV yaw pitch stroke)
                                (Value.numField "yaw" fs)
                                (Value.numField "pitch" fs)
                                (Value.strField "stroke" fs)
                        )
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"sphere\"\n    , l = "
        ++ String.fromInt d.l
        ++ "\n    , m = "
        ++ String.fromInt d.m
        ++ "\n    , bump = "
        ++ Value.numStr d.bump
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
            clamp 8 48 d.segU

        vs =
            clamp 5 40 d.segV

        point ui vj =
            let
                u =
                    2 * pi * toFloat ui / toFloat us

                v =
                    pi * toFloat vj / toFloat (vs - 1)

                rad =
                    1 + d.bump * sin (toFloat d.l * v) * cos (toFloat d.m * u)
            in
            { x = rad * sin v * cos u, y = rad * sin v * sin u, z = rad * cos v }

        grid =
            List.concatMap (\ui -> List.map (point ui) (List.range 0 (vs - 1))) (List.range 0 (us - 1))
    in
    \mode phase -> Draw.surface (d.yaw + phase) d.pitch us vs True False grid (Color.solid (Color.resolve mode d.stroke) phase)



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a spherical harmonic yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Harmonic"
                    [ Form.slider "l (latitude)" 0 9 1 (toFloat d.l) (\v -> toSource { d | l = round v })
                    , Form.slider "m (longitude)" 0 9 1 (toFloat d.m) (\v -> toSource { d | m = round v })
                    , Form.slider "bump" 0 0.9 0.02 d.bump (\v -> toSource { d | bump = v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Mesh"
                    [ Form.slider "longitude lines" 8 48 1 (toFloat d.segU) (\v -> toSource { d | segU = round v })
                    , Form.slider "latitude lines" 5 40 1 (toFloat d.segV) (\v -> toSource { d | segV = round v })
                    , Form.slider "pitch" 0 6.2832 0.01 d.pitch (\v -> toSource { d | pitch = v })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Sphere" (toSource { d | bump = 0 })
                        , Form.preset "4 · 4" (toSource { d | l = 4, m = 4, bump = 0.45 })
                        , Form.preset "3 · 3" (toSource { d | l = 3, m = 3, bump = 0.5 })
                        , Form.preset "6 · 2" (toSource { d | l = 6, m = 2, bump = 0.4 })
                        , Form.preset "2 · 6" (toSource { d | l = 2, m = 6, bump = 0.4 })
                        , Form.preset "8 · 5" (toSource { d | l = 8, m = 5, bump = 0.3 })
                        ]
                    ]
                ]
