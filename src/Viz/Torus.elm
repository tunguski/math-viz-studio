module Viz.Torus exposing (viz)

{-| **Torus** — a doughnut surface drawn as a wireframe of two families of circles: the big loops
around the hole (radius `bigR`) and the small tube cross-sections (radius `smallR`). Rotated and
projected to the plane; the yaw spins with the animation clock.
-}

import Array exposing (Array)
import Color
import Draw exposing (Vec3)
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as A
import Value
import Viz exposing (Viz)


type alias Model =
    { bigR : Float
    , smallR : Float
    , segU : Int
    , segV : Int
    , yaw : Float
    , pitch : Float
    , stroke : String
    }


viz : Viz
viz =
    { kind = "torus"
    , name = "Torus"
    , description = "A doughnut surface as a spinning wireframe of circles."
    , about = "The torus — a doughnut, or the surface of a bagel — is one of the fundamental surfaces of topology, the simplest after the sphere and the canonical example of a shape with a hole. It is where a video-game world that \"wraps around\" lives, and the natural home of doubly-periodic things like the two angles of a double pendulum.\n\nParametrised by two angles, it is built from two families of circles: big loops around the central hole and small loops around the tube. Drawing those circles as a rotating wireframe is a classic way to feel a curved surface sitting in three dimensions."
    , starter = toSource default
    , movable = True
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { bigR = 2, smallR = 0.8, segU = 24, segV = 16, yaw = 0.6, pitch = 1.0, stroke = "#5fd0ff" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map5 (\bigR smallR segU segV yaw -> ( bigR, smallR, ( segU, segV, yaw ) ))
                    (Value.numField "bigR" fs)
                    (Value.numField "smallR" fs)
                    (Value.intField "segU" fs)
                    (Value.intField "segV" fs)
                    (Value.numField "yaw" fs)
                    |> Result.andThen
                        (\( bigR, smallR, ( segU, segV, yaw ) ) ->
                            Result.map2 (\pitch stroke -> Model bigR smallR segU segV yaw pitch stroke)
                                (Value.numField "pitch" fs)
                                (Value.strField "stroke" fs)
                        )
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"torus\"\n"
        ++ "    , bigR = "
        ++ Value.numStr d.bigR
        ++ "\n    , smallR = "
        ++ Value.numStr d.smallR
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
            clamp 6 48 d.segU

        vs =
            clamp 4 32 d.segV

        point3 ui vj =
            let
                u =
                    2 * pi * toFloat ui / toFloat us

                v =
                    2 * pi * toFloat vj / toFloat vs

                rr =
                    d.bigR + d.smallR * cos v
            in
            { x = rr * cos u, y = rr * sin u, z = d.smallR * sin v }

        grid =
            List.concatMap (\ui -> List.map (point3 ui) (List.range 0 (vs - 1))) (List.range 0 (us - 1))
    in
    \mode phase -> draw d us vs grid (Color.solid (Color.resolve mode d.stroke) phase) phase


draw : Model -> Int -> Int -> List Vec3 -> String -> Float -> Svg msg
draw d us vs grid color phase =
    let
        proj =
            List.map (Draw.rotate2 (d.yaw + phase) d.pitch) grid

        ( cx, cy, scale ) =
            Draw.fitTransform proj

        arr =
            Array.fromList (List.map (\( x, y ) -> ( (x - cx) * scale, -(y - cy) * scale )) proj)

        at ui vj =
            case Array.get (ui * vs + vj) arr of
                Just ( x, y ) ->
                    Draw.r1 x ++ " " ++ Draw.r1 y

                Nothing ->
                    "0 0"

        uRing ui =
            "M" ++ at ui 0 ++ String.concat (List.map (\vj -> "L" ++ at ui vj) (List.range 1 (vs - 1))) ++ "L" ++ at ui 0

        vRing vj =
            "M" ++ at 0 vj ++ String.concat (List.map (\ui -> "L" ++ at ui vj) (List.range 1 (us - 1))) ++ "L" ++ at 0 vj

        path =
            String.concat (List.map uRing (List.range 0 (us - 1)) ++ List.map vRing (List.range 0 (vs - 1)))
    in
    Draw.stage [ Svg.path [ A.d path, A.fill "none", A.stroke color, A.strokeWidth "1", A.opacity "0.85", A.strokeLinejoin "round" ] [] ]



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a torus yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Shape"
                    [ Form.slider "ring radius" 0.4 3 0.05 d.bigR (\v -> toSource { d | bigR = v })
                    , Form.slider "tube radius" 0.2 2 0.05 d.smallR (\v -> toSource { d | smallR = v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Mesh"
                    [ Form.slider "loops" 8 48 1 (toFloat d.segU) (\v -> toSource { d | segU = round v })
                    , Form.slider "tube divisions" 4 32 1 (toFloat d.segV) (\v -> toSource { d | segV = round v })
                    , Form.slider "pitch" 0 6.2832 0.01 d.pitch (\v -> toSource { d | pitch = v })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Ring" (toSource { d | bigR = 2, smallR = 0.7 })
                        , Form.preset "Fat" (toSource { d | bigR = 1.5, smallR = 1.1 })
                        , Form.preset "Thin" (toSource { d | bigR = 2.2, smallR = 0.35 })
                        , Form.preset "Horn" (toSource { d | bigR = 0.7, smallR = 1.0 })
                        , Form.preset "Spindle" (toSource { d | bigR = 0.4, smallR = 1.2 })
                        , Form.preset "Fine mesh" (toSource { d | segU = 40, segV = 26 })
                        ]
                    ]
                ]
