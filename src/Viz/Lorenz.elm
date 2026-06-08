module Viz.Lorenz exposing (viz)

{-| **Lorenz attractor** — the chaotic flow `x' = σ(y−x)`, `y' = x(ρ−z) − y`, `z' = xy − βz`,
Euler-integrated and projected (and spun by yaw, which the animation clock drives).
-}

import Draw exposing (Vec3)
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as A
import Value
import Viz exposing (Viz)


type alias Model =
    { sigma : Float
    , rho : Float
    , beta : Float
    , dt : Float
    , steps : Int
    , yaw : Float
    , pitch : Float
    , stroke : String
    }


viz : Viz
viz =
    { kind = "lorenz"
    , name = "Lorenz attractor"
    , description = "A strange attractor — the chaotic butterfly flow."
    , starter = toSource default
    , movable = True
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { sigma = 10, rho = 28, beta = 2.6667, dt = 0.006, steps = 9000, yaw = 0.6, pitch = 0.2, stroke = "#ffb347" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map5 (\sigma rho beta dt steps -> ( sigma, rho, ( beta, dt, steps ) ))
                    (Value.numField "sigma" fs)
                    (Value.numField "rho" fs)
                    (Value.numField "beta" fs)
                    (Value.numField "dt" fs)
                    (Value.intField "steps" fs)
                    |> Result.andThen
                        (\( sigma, rho, ( beta, dt, steps ) ) ->
                            Result.map3 (\yaw pitch stroke -> Model sigma rho beta dt steps yaw pitch stroke)
                                (Value.numField "yaw" fs)
                                (Value.numField "pitch" fs)
                                (Value.strField "stroke" fs)
                        )
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"lorenz\"\n"
        ++ "    , sigma = "
        ++ Value.numStr d.sigma
        ++ "\n    , rho = "
        ++ Value.numStr d.rho
        ++ "\n    , beta = "
        ++ Value.numStr d.beta
        ++ "\n    , dt = "
        ++ Value.numStr d.dt
        ++ "\n    , steps = "
        ++ String.fromInt d.steps
        ++ "\n    , yaw = "
        ++ Value.numStr d.yaw
        ++ "\n    , pitch = "
        ++ Value.numStr d.pitch
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"



-- RENDER ------------------------------------------------------------------------------------------


{-| Integrate and centre the trajectory **once**, returning a drawer that only rotates, projects and
stringifies per frame (so animation never re-integrates 9000 Euler steps). -}
prepare : Model -> (Float -> Svg msg)
prepare d =
    let
        path3 =
            integrate d

        c =
            Draw.centroid path3

        centered =
            List.map (\p -> { x = p.x - c.x, y = p.y - c.y, z = p.z - c.z }) path3
    in
    \phase -> draw d centered phase


draw : Model -> List Vec3 -> Float -> Svg msg
draw d centered phase =
    let
        proj =
            List.map (Draw.rotate2 (d.yaw + phase) d.pitch) centered

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
            , A.strokeWidth "1"
            , A.strokeLinecap "round"
            , A.opacity "0.9"
            ]
            []
        ]


{-| Euler-integrate the Lorenz system from a point just off the origin. -}
integrate : Model -> List Vec3
integrate d =
    let
        steps =
            clamp 100 20000 d.steps

        step _ ( p, acc ) =
            let
                np =
                    { x = p.x + d.sigma * (p.y - p.x) * d.dt
                    , y = p.y + (p.x * (d.rho - p.z) - p.y) * d.dt
                    , z = p.z + (p.x * p.y - d.beta * p.z) * d.dt
                    }
            in
            ( np, np :: acc )

        ( _, pts ) =
            List.foldl step ( { x = 0.1, y = 0, z = 0 }, [] ) (List.range 1 steps)
    in
    List.reverse pts



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a Lorenz attractor yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Flow"
                    [ Form.slider "sigma (σ)" 0 20 0.1 d.sigma (\v -> toSource { d | sigma = v })
                    , Form.slider "rho (ρ)" 0 100 0.1 d.rho (\v -> toSource { d | rho = v })
                    , Form.slider "beta (β)" 0 5 0.01 d.beta (\v -> toSource { d | beta = v })
                    ]
                , Form.group "Integration"
                    [ Form.slider "dt" 0.001 0.02 0.001 d.dt (\v -> toSource { d | dt = v })
                    , Form.slider "steps" 1000 16000 500 (toFloat d.steps) (\v -> toSource { d | steps = round v })
                    ]
                , Form.group "View"
                    [ Form.slider "pitch" 0 6.2832 0.01 d.pitch (\v -> toSource { d | pitch = v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Classic ρ=28" (toSource { d | rho = 28 })
                        , Form.preset "Stable ρ=14" (toSource { d | rho = 14 })
                        , Form.preset "Wild ρ=99.96" (toSource { d | rho = 99.96 })
                        ]
                    ]
                , Form.hint "The attractor spins while it animates (yaw is the clock)."
                ]
