module Viz.LSystem exposing (viz)

{-| **L-system** — a tiny formal grammar drawn by a turtle. Start from `axiom`, rewrite it
`iterations` times by the `rules`, then walk the result: `F`/`G`/`A`/`B` draw forward, `+`/`−` turn by
`angle`, `[`/`]` push/pop the turtle's state (for branches). The model *is* a grammar, so a few
characters grow a Koch snowflake, a dragon curve or a plant.
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
    { axiom : String
    , rules : List ( String, String )
    , angle : Float
    , iterations : Int
    , stroke : String
    }


viz : Viz
viz =
    { kind = "lsystem"
    , name = "L-system"
    , description = "A rewriting grammar drawn by a turtle — snowflakes, dragons, plants."
    , starter = toSource plant
    , movable = False
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map5 Model
                    (Value.strField "axiom" fs)
                    (Value.listField "rules" fs |> Result.andThen (Value.traverse rule))
                    (Value.numField "angle" fs)
                    (Value.intField "iterations" fs)
                    (Value.strField "stroke" fs)
            )


rule : Value.V -> Result String ( String, String )
rule v =
    Value.tuple v
        |> Result.andThen
            (\xs ->
                case xs of
                    [ a, b ] ->
                        Result.map2 Tuple.pair (Value.str a) (Value.str b)

                    _ ->
                        Err "Expected a 2-tuple rule like ( \"F\", \"F+F\" )"
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"lsystem\"\n"
        ++ "    , axiom = \""
        ++ d.axiom
        ++ "\"\n    , rules =\n        "
        ++ Value.renderList "        " (List.map ruleStr d.rules)
        ++ "\n    , angle = "
        ++ Value.numStr d.angle
        ++ "\n    , iterations = "
        ++ String.fromInt d.iterations
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"


ruleStr : ( String, String ) -> String
ruleStr ( from, to ) =
    "( \"" ++ from ++ "\", \"" ++ to ++ "\" )"



-- RENDER ------------------------------------------------------------------------------------------


prepare : Model -> (String -> Float -> Svg msg)
prepare d =
    let
        expanded =
            expand d.rules (clamp 0 8 d.iterations) d.axiom

        pts =
            turtle (degrees d.angle) (String.toList expanded)

        coords =
            List.map (\( _, x, y ) -> ( x, y )) pts

        ( cx, cy, scale ) =
            Draw.fitTransform coords

        path =
            String.concat
                (List.map
                    (\( move, x, y ) ->
                        (if move then
                            "M"

                         else
                            "L"
                        )
                            ++ Draw.r1 ((x - cx) * scale)
                            ++ " "
                            ++ Draw.r1 (-(y - cy) * scale)
                    )
                    pts
                )
    in
    \mode phase ->
        Draw.stage
            [ Svg.path
                [ A.d path
                , A.fill "none"
                , A.stroke (Color.solid (Color.resolve mode d.stroke) phase)
                , A.strokeWidth "1"
                , A.strokeLinejoin "round"
                , A.strokeLinecap "round"
                , A.opacity "0.9"
                ]
                []
            ]


{-| Rewrite the string `n` times, each character replaced by its rule (or kept). Guarded against
runaway growth. -}
expand : List ( String, String ) -> Int -> String -> String
expand rules n s =
    if n <= 0 || String.length s > 200000 then
        s

    else
        expand rules (n - 1) (String.concat (List.map (replace rules) (String.toList s)))


replace : List ( String, String ) -> Char -> String
replace rules c =
    case rules of
        ( from, to ) :: rest ->
            if from == String.fromChar c then
                to

            else
                replace rest c

        [] ->
            String.fromChar c


{-| Walk the expanded string, returning `(needsMoveTo, x, y)` path points. -}
turtle : Float -> List Char -> List ( Bool, Float, Float )
turtle angle chars =
    let
        forward c =
            c == 'F' || c == 'G' || c == 'A' || c == 'B'

        go cs st =
            case cs of
                [] ->
                    List.reverse st.pts

                c :: rest ->
                    go rest (stepChar angle forward c st)
    in
    go chars { x = 0, y = 0, h = pi / 2, up = True, stack = [], pts = [] }


stepChar :
    Float
    -> (Char -> Bool)
    -> Char
    -> { x : Float, y : Float, h : Float, up : Bool, stack : List ( Float, Float, Float ), pts : List ( Bool, Float, Float ) }
    -> { x : Float, y : Float, h : Float, up : Bool, stack : List ( Float, Float, Float ), pts : List ( Bool, Float, Float ) }
stepChar angle forward c st =
    if forward c then
        let
            nx =
                st.x + cos st.h

            ny =
                st.y + sin st.h

            pts2 =
                if st.up then
                    ( False, nx, ny ) :: ( True, st.x, st.y ) :: st.pts

                else
                    ( False, nx, ny ) :: st.pts
        in
        { st | x = nx, y = ny, up = False, pts = pts2 }

    else if c == '+' then
        { st | h = st.h + angle }

    else if c == '-' then
        { st | h = st.h - angle }

    else if c == '[' then
        { st | stack = ( st.x, st.y, st.h ) :: st.stack }

    else if c == ']' then
        case st.stack of
            ( x, y, h ) :: rest ->
                { st | x = x, y = y, h = h, up = True, stack = rest }

            [] ->
                st

    else
        st



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't an L-system yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Grammar"
                    [ Form.slider "angle" 0 120 0.5 d.angle (\v -> toSource { d | angle = v })
                    , Form.slider "iterations" 0 8 1 (toFloat d.iterations) (\v -> toSource { d | iterations = round v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Plant" (toSource plant)
                        , Form.preset "Koch" (toSource koch)
                        , Form.preset "Dragon" (toSource dragon)
                        , Form.preset "Arrowhead" (toSource arrowhead)
                        , Form.preset "Sierpiński" (toSource sierpinski)
                        , Form.preset "Gosper" (toSource gosper)
                        , Form.preset "Bush" (toSource bush)
                        ]
                    ]
                , Form.hint "Edit the axiom and rules in the Code tab — F/G/A/B draw, +/- turn, [ ] branch."
                ]



-- PRESETS -----------------------------------------------------------------------------------------


plant : Model
plant =
    { axiom = "X"
    , rules = [ ( "X", "F+[[X]-X]-F[-FX]+X" ), ( "F", "FF" ) ]
    , angle = 25
    , iterations = 5
    , stroke = "#86efac"
    }


koch : Model
koch =
    { axiom = "F--F--F"
    , rules = [ ( "F", "F+F--F+F" ) ]
    , angle = 60
    , iterations = 4
    , stroke = "#67e8f9"
    }


dragon : Model
dragon =
    { axiom = "FX"
    , rules = [ ( "X", "X+YF+" ), ( "Y", "-FX-Y" ) ]
    , angle = 90
    , iterations = 11
    , stroke = "#fca5f1"
    }


arrowhead : Model
arrowhead =
    { axiom = "A"
    , rules = [ ( "A", "B-A-B" ), ( "B", "A+B+A" ) ]
    , angle = 60
    , iterations = 6
    , stroke = "#fbbf24"
    }


sierpinski : Model
sierpinski =
    { axiom = "F-G-G"
    , rules = [ ( "F", "F-G+F+G-F" ), ( "G", "GG" ) ]
    , angle = 120
    , iterations = 5
    , stroke = "#fca5f1"
    }


gosper : Model
gosper =
    { axiom = "A"
    , rules = [ ( "A", "A-B--B+A++AA+B-" ), ( "B", "+A-BB--B-A++A+B" ) ]
    , angle = 60
    , iterations = 4
    , stroke = "#7dd3fc"
    }


bush : Model
bush =
    { axiom = "F"
    , rules = [ ( "F", "FF+[+F-F-F]-[-F+F+F]" ) ]
    , angle = 22.5
    , iterations = 4
    , stroke = "#86efac"
    }
