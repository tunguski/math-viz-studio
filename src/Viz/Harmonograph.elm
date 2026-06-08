module Viz.Harmonograph exposing (viz)

{-| **Harmonograph** — two lists of damped oscillators driving the x and y of a pen. The traced curve
is `x(t) = Σ amp·sin(freq·t + phase)·e^(−decay·t)` (and likewise for y), sampled `samples` times. A
self-contained visualisation: its model, its decode/print, its render and its controls live here, and
it exposes one `viz : Viz`.
-}

import Draw
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as A
import Value exposing (V)
import Viz exposing (Viz)


{-| The model. -}
type alias Model =
    { x : List Pendulum
    , y : List Pendulum
    , samples : Int
    , stroke : String
    }


{-| One damped sinusoid. -}
type alias Pendulum =
    { amp : Float, freq : Float, phase : Float, decay : Float }


{-| The plugin record the registry hosts. -}
viz : Viz
viz =
    { kind = "harmonograph"
    , name = "Harmonograph"
    , description = "Sums of damped sinusoids — looping plane curves."
    , starter = toSource default
    , movable = True
    , render = \phase source -> Result.map (view phase) (decode source)
    , controls = controls
    }


default : Model
default =
    { x =
        [ { amp = 150, freq = 3, phase = 0, decay = 0.004 }
        , { amp = 90, freq = 2, phase = 1.5708, decay = 0.008 }
        ]
    , y =
        [ { amp = 150, freq = 2, phase = 0, decay = 0.004 }
        , { amp = 90, freq = 3, phase = 2.094, decay = 0.008 }
        ]
    , samples = 6000
    , stroke = "#7cdcff"
    }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map4 Model
                    (Value.listField "x" fs |> Result.andThen (Value.traverse pendulum))
                    (Value.listField "y" fs |> Result.andThen (Value.traverse pendulum))
                    (Value.intField "samples" fs)
                    (Value.strField "stroke" fs)
            )


pendulum : V -> Result String Pendulum
pendulum v =
    Value.record v
        |> Result.andThen
            (\fs ->
                Result.map4 Pendulum
                    (Value.numField "amp" fs)
                    (Value.numField "freq" fs)
                    (Value.numField "phase" fs)
                    (Value.numField "decay" fs)
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"harmonograph\"\n"
        ++ "    , x =\n        "
        ++ Value.renderList "        " (List.map pendulumStr d.x)
        ++ "\n    , y =\n        "
        ++ Value.renderList "        " (List.map pendulumStr d.y)
        ++ "\n    , samples = "
        ++ String.fromInt d.samples
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"


pendulumStr : Pendulum -> String
pendulumStr p =
    let
        n =
            Value.numStr
    in
    "{ amp = " ++ n p.amp ++ ", freq = " ++ n p.freq ++ ", phase = " ++ n p.phase ++ ", decay = " ++ n p.decay ++ " }"



-- RENDER ------------------------------------------------------------------------------------------


view : Float -> Model -> Svg msg
view phase d =
    let
        count =
            max 2 d.samples

        -- the whole curve is traced over this many radians of t; the decay makes it spiral inward
        tSpan =
            220.0

        xs =
            shiftPhase phase d.x

        ys =
            shiftPhase phase d.y

        point i =
            let
                t =
                    toFloat i / toFloat (count - 1) * tSpan
            in
            Draw.r2 (sumPend t xs) ++ "," ++ Draw.r2 (sumPend t ys)

        pts =
            String.join " " (List.map point (List.range 0 (count - 1)))
    in
    Draw.stage
        [ Svg.polyline
            [ A.points pts
            , A.fill "none"
            , A.stroke d.stroke
            , A.strokeWidth "1.1"
            , A.opacity "0.92"
            ]
            []
        ]


sumPend : Float -> List Pendulum -> Float
sumPend t ps =
    List.foldl (\p acc -> acc + p.amp * sin (p.freq * t + p.phase) * e ^ (-p.decay * t)) 0 ps


{-| Slowly precess every oscillator by the animation phase (a no-op at phase 0). -}
shiftPhase : Float -> List Pendulum -> List Pendulum
shiftPhase phase ps =
    List.map (\p -> { p | phase = p.phase + phase }) ps



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a harmonograph yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Trace"
                    [ Form.slider "samples" 200 12000 100 (toFloat d.samples) (\v -> toSource { d | samples = round v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , oscGroup "X oscillators" (\xs -> { d | x = xs }) d.x
                , oscGroup "Y oscillators" (\ys -> { d | y = ys }) d.y
                ]


oscGroup : String -> (List Pendulum -> Model) -> List Pendulum -> Html String
oscGroup title set ps =
    Form.group title (List.concat (List.indexedMap (pendulumRow set ps) ps))


pendulumRow : (List Pendulum -> Model) -> List Pendulum -> Int -> Pendulum -> List (Html String)
pendulumRow set ps i p =
    let
        on f v =
            toSource (set (Form.updateAt i (f v) ps))
    in
    [ Form.subhead ("oscillator " ++ String.fromInt (i + 1))
    , Form.slider "amplitude" 0 220 1 p.amp (on (\v q -> { q | amp = v }))
    , Form.slider "frequency" 0.5 6 0.01 p.freq (on (\v q -> { q | freq = v }))
    , Form.slider "phase" 0 6.2832 0.01 p.phase (on (\v q -> { q | phase = v }))
    , Form.slider "decay" 0 0.02 0.0005 p.decay (on (\v q -> { q | decay = v }))
    ]
