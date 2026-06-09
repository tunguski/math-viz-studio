module Viz.FordCircles exposing (viz)

{-| **Ford circles** — a circle packing built from the fractions: for every reduced fraction p/q in
[0, 1], draw a circle of radius 1/(2q²) sitting on the number line at p/q. Any two of them are either
disjoint or exactly tangent, never overlapping — a picture of the rationals.
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
    { maxQ : Int
    , stroke : String
    }


viz : Viz
viz =
    { kind = "ford"
    , name = "Ford circles"
    , description = "A circle packing of the fractions — tangent, never overlapping."
    , about = "For each fraction p/q in lowest terms, Lester Ford (1938) drew a circle of radius 1/(2q²) resting on the number line at p/q. The magic: two Ford circles never cross — they are tangent exactly when their fractions are \"neighbours\" in a Farey sequence (when |p·s − r·q| = 1).\n\nSo this tidy packing of circles is really a picture of the rational numbers and how well they approximate each other, tying together number theory (Farey sequences, the Stern–Brocot tree, continued fractions) and the geometry of tangent circles."
    , starter = toSource default
    , movable = False
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { maxQ = 14, stroke = "#5fd0ff" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map2 Model
                    (Value.intField "maxQ" fs)
                    (Value.strField "stroke" fs)
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"ford\"\n    , maxQ = "
        ++ String.fromInt d.maxQ
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"



-- RENDER ------------------------------------------------------------------------------------------


prepare : Model -> (String -> Float -> Svg msg)
prepare d =
    let
        maxQ =
            clamp 2 40 d.maxQ

        scale =
            560

        fractions =
            List.concatMap
                (\q -> List.filterMap (\p -> if gcd p q == 1 then Just ( p, q ) else Nothing) (List.range 0 q))
                (List.range 1 maxQ)
    in
    \mode phase ->
        let
            coloring =
                Color.resolve mode d.stroke

            circ ( p, q ) =
                let
                    r =
                        1 / (2 * toFloat q * toFloat q)
                in
                Svg.circle
                    [ A.cx (Draw.r1 (toFloat p / toFloat q * scale - 280))
                    , A.cy (Draw.r1 (280 - r * scale))
                    , A.r (Draw.r1 (r * scale))
                    , A.fill "none"
                    , A.stroke (Color.sample coloring (toFloat q / toFloat maxQ) phase)
                    , A.strokeWidth "1.2"
                    , A.opacity "0.85"
                    ]
                    []
        in
        Draw.stage (List.map circ fractions)


gcd : Int -> Int -> Int
gcd a b =
    if b == 0 then
        a

    else
        gcd b (modBy b a)



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a Ford-circle packing yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Packing"
                    [ Form.slider "max denominator" 2 40 1 (toFloat d.maxQ) (\v -> toSource { d | maxQ = round v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Coarse 6" (toSource { d | maxQ = 6 })
                        , Form.preset "10" (toSource { d | maxQ = 10 })
                        , Form.preset "14" (toSource { d | maxQ = 14 })
                        , Form.preset "20" (toSource { d | maxQ = 20 })
                        , Form.preset "Fine 30" (toSource { d | maxQ = 30 })
                        ]
                    ]
                , Form.hint "Try the Gradient colour mode to shade circles by denominator."
                ]
