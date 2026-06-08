module Viz.Automaton exposing (viz)

{-| **Cellular automaton** — an elementary Wolfram `rule` (0–255) evolved for `generations` rows over
a strip `width` cells wide, from the `seed` cells (column offsets from the centre). The space-time
diagram is drawn as one filled `<path>`.
-}

import Array exposing (Array)
import Draw
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as A
import Value
import Viz exposing (Viz)


type alias Model =
    { rule : Int
    , width : Int
    , generations : Int
    , seed : List Int
    , stroke : String
    }


viz : Viz
viz =
    { kind = "automaton"
    , name = "Cellular automaton"
    , description = "An elementary Wolfram rule, drawn row by row."
    , starter = toSource default
    , movable = False
    , render = \source -> Result.map (\m -> always (view m)) (decode source)
    , controls = controls
    }


default : Model
default =
    { rule = 30, width = 121, generations = 70, seed = [ 0 ], stroke = "#9b8cff" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map5 Model
                    (Value.intField "rule" fs)
                    (Value.intField "width" fs)
                    (Value.intField "generations" fs)
                    (Value.listField "seed" fs |> Result.andThen (Value.traverse Value.int))
                    (Value.strField "stroke" fs)
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"automaton\"\n"
        ++ "    , rule = "
        ++ String.fromInt d.rule
        ++ "\n    , width = "
        ++ String.fromInt d.width
        ++ "\n    , generations = "
        ++ String.fromInt d.generations
        ++ "\n    , seed = "
        ++ Value.renderList "        " (List.map String.fromInt d.seed)
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"



-- RENDER ------------------------------------------------------------------------------------------


view : Model -> Svg msg
view d =
    let
        w =
            clamp 5 251 d.width

        gens =
            clamp 1 200 d.generations

        center =
            w // 2

        row0 =
            Array.fromList (List.map (\col -> List.member (col - center) d.seed) (List.range 0 (w - 1)))

        rows =
            buildRows d.rule gens row0

        cell =
            580 / toFloat (max w gens)

        ox =
            -(toFloat w * cell) / 2

        oy =
            -(toFloat gens * cell) / 2

        squares =
            String.concat
                (List.concat
                    (List.indexedMap
                        (\g rowArr ->
                            List.filterMap
                                (\col ->
                                    if Maybe.withDefault False (Array.get col rowArr) then
                                        Just (square (ox + toFloat col * cell) (oy + toFloat g * cell) cell)

                                    else
                                        Nothing
                                )
                                (List.range 0 (w - 1))
                        )
                        rows
                    )
                )
    in
    Draw.stage [ Svg.path [ A.d squares, A.fill d.stroke, A.opacity "0.95" ] [] ]


square : Float -> Float -> Float -> String
square x y s =
    "M" ++ Draw.r1 x ++ " " ++ Draw.r1 y ++ "h" ++ Draw.r1 s ++ "v" ++ Draw.r1 s ++ "h" ++ Draw.r1 (-s) ++ "z"


{-| Evolve `gens` rows from `row0` under the elementary rule. -}
buildRows : Int -> Int -> Array Bool -> List (Array Bool)
buildRows rule gens row0 =
    let
        go remaining cur acc =
            if remaining <= 0 then
                List.reverse acc

            else
                go (remaining - 1) (nextRow rule cur) (cur :: acc)
    in
    go gens row0 []


nextRow : Int -> Array Bool -> Array Bool
nextRow rule cur =
    let
        w =
            Array.length cur

        bitAt i =
            if Maybe.withDefault False (Array.get i cur) then
                1

            else
                0
    in
    Array.fromList
        (List.map
            (\i -> ruleBit rule (4 * bitAt (i - 1) + 2 * bitAt i + bitAt (i + 1)))
            (List.range 0 (w - 1))
        )


ruleBit : Int -> Int -> Bool
ruleBit rule idx =
    modBy 2 (rule // (2 ^ idx)) == 1



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a cellular automaton yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Rule"
                    [ Form.slider "rule" 0 255 1 (toFloat d.rule) (\v -> toSource { d | rule = round v })
                    , Form.presets
                        [ Form.preset "30" (toSource { d | rule = 30 })
                        , Form.preset "90" (toSource { d | rule = 90 })
                        , Form.preset "110" (toSource { d | rule = 110 })
                        , Form.preset "184" (toSource { d | rule = 184 })
                        ]
                    ]
                , Form.group "Grid"
                    [ Form.slider "width" 21 201 2 (toFloat d.width) (\v -> toSource { d | width = round v })
                    , Form.slider "generations" 10 160 1 (toFloat d.generations) (\v -> toSource { d | generations = round v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.hint "Seed cells (column offsets from centre) live in the Code tab."
                ]
