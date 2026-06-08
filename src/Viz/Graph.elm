module Viz.Graph exposing (viz)

{-| **Force-directed graph** — named nodes and the edges between them, laid out by a
Fruchterman–Reingold spring simulation. The model *is* the graph.
-}

import Array exposing (Array)
import Color
import Draw
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as A
import Value
import Viz exposing (Viz)


type alias Model =
    { nodes : List String
    , edges : List ( Int, Int )
    , iterations : Int
    , stroke : String
    }


viz : Viz
viz =
    { kind = "graph"
    , name = "Force-directed graph"
    , description = "A network laid out by a spring simulation."
    , starter = toSource wheel
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
                Result.map4 Model
                    (Value.listField "nodes" fs |> Result.andThen (Value.traverse Value.str))
                    (Value.listField "edges" fs |> Result.andThen (Value.traverse edge))
                    (Value.intField "iterations" fs)
                    (Value.strField "stroke" fs)
            )


edge : Value.V -> Result String ( Int, Int )
edge v =
    Value.tuple v
        |> Result.andThen
            (\xs ->
                case xs of
                    [ a, b ] ->
                        Result.map2 Tuple.pair (Value.int a) (Value.int b)

                    _ ->
                        Err "Expected a 2-tuple edge like ( 0, 1 )"
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"graph\"\n"
        ++ "    , nodes =\n        "
        ++ Value.renderList "        " (List.map (\name -> "\"" ++ name ++ "\"") d.nodes)
        ++ "\n    , edges =\n        "
        ++ Value.renderList "        " (List.map edgeStr d.edges)
        ++ "\n    , iterations = "
        ++ String.fromInt d.iterations
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"


edgeStr : ( Int, Int ) -> String
edgeStr ( i, j ) =
    "( " ++ String.fromInt i ++ ", " ++ String.fromInt j ++ " )"



-- RENDER ------------------------------------------------------------------------------------------


{-| Run the spring layout **once**; the drawer only recolours the edges and nodes. -}
prepare : Model -> (String -> Float -> Svg msg)
prepare d =
    let
        positions =
            layout (List.length d.nodes) d.edges (clamp 1 400 d.iterations)

        ( cx, cy, scale ) =
            Draw.fitTransform positions

        placed =
            Array.fromList (List.map (\( x, y ) -> ( (x - cx) * scale, -(y - cy) * scale )) positions)
    in
    \mode phase -> draw d placed (Color.solid (Color.resolve mode d.stroke) phase)


draw : Model -> Array ( Float, Float ) -> String -> Svg msg
draw d placed color =
    let
        line ( a, b ) =
            case ( Array.get a placed, Array.get b placed ) of
                ( Just ( x1, y1 ), Just ( x2, y2 ) ) ->
                    Just
                        (Svg.line
                            [ A.x1 (Draw.r2 x1)
                            , A.y1 (Draw.r2 y1)
                            , A.x2 (Draw.r2 x2)
                            , A.y2 (Draw.r2 y2)
                            , A.stroke color
                            , A.strokeWidth "1.6"
                            , A.opacity "0.5"
                            ]
                            []
                        )

                _ ->
                    Nothing

        node label ( x, y ) =
            [ Svg.circle [ A.cx (Draw.r2 x), A.cy (Draw.r2 y), A.r "13", A.fill color, A.opacity "0.95" ] []
            , Svg.text_
                [ A.x (Draw.r2 x)
                , A.y (Draw.r2 y)
                , A.textAnchor "middle"
                , A.dominantBaseline "central"
                , A.fontSize "11"
                , A.fill "#0b0e14"
                ]
                [ Svg.text label ]
            ]
    in
    Draw.stage
        (List.filterMap line d.edges
            ++ List.concat (List.map2 node d.nodes (Array.toList placed))
        )


{-| A Fruchterman–Reingold spring layout: nodes start on a circle, then relax under edge attraction
and all-pairs repulsion, cooling each round. Returns a position per node (index-aligned). -}
layout : Int -> List ( Int, Int ) -> Int -> List ( Float, Float )
layout nN edges iters =
    let
        k =
            1.0

        init =
            List.map
                (\i -> let a = 2 * pi * toFloat i / toFloat (max 1 nN) in ( cos a, sin a ))
                (List.range 0 (nN - 1))

        go temp positions remaining =
            if remaining <= 0 then
                positions

            else
                go (temp * 0.95) (frStep edges k temp positions) (remaining - 1)
    in
    go (k * 0.4) init iters


frStep : List ( Int, Int ) -> Float -> Float -> List ( Float, Float ) -> List ( Float, Float )
frStep edges k temp positions =
    let
        arr =
            Array.fromList positions

        nN =
            Array.length arr

        getp i =
            Maybe.withDefault ( 0, 0 ) (Array.get i arr)

        repDisp =
            Array.fromList
                (List.map
                    (\i ->
                        List.foldl
                            (\j ( dx, dy ) ->
                                if j == i then
                                    ( dx, dy )

                                else
                                    let
                                        ( xi, yi ) =
                                            getp i

                                        ( xj, yj ) =
                                            getp j

                                        ux =
                                            xi - xj

                                        uy =
                                            yi - yj

                                        dist =
                                            sqrt (ux * ux + uy * uy) + 0.001

                                        force =
                                            k * k / dist
                                    in
                                    ( dx + ux / dist * force, dy + uy / dist * force )
                            )
                            ( 0, 0 )
                            (List.range 0 (nN - 1))
                    )
                    (List.range 0 (nN - 1))
                )

        disp =
            List.foldl
                (\( a, b ) acc ->
                    let
                        ( xa, ya ) =
                            getp a

                        ( xb, yb ) =
                            getp b

                        ux =
                            xa - xb

                        uy =
                            ya - yb

                        dist =
                            sqrt (ux * ux + uy * uy) + 0.001

                        force =
                            dist * dist / k

                        fx =
                            ux / dist * force

                        fy =
                            uy / dist * force

                        bump idx gx gy d2 =
                            case Array.get idx d2 of
                                Just ( dx, dy ) ->
                                    Array.set idx ( dx + gx, dy + gy ) d2

                                Nothing ->
                                    d2
                    in
                    bump b fx fy (bump a (-fx) (-fy) acc)
                )
                repDisp
                edges
    in
    List.indexedMap
        (\i ( x, y ) ->
            case Array.get i disp of
                Just ( dx, dy ) ->
                    let
                        mag =
                            sqrt (dx * dx + dy * dy) + 0.0001

                        lim =
                            min mag temp
                    in
                    ( x + dx / mag * lim, y + dy / mag * lim )

                Nothing ->
                    ( x, y )
        )
        positions



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a graph yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Layout"
                    [ Form.slider "iterations" 10 300 5 (toFloat d.iterations) (\v -> toSource { d | iterations = round v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Wheel" (toSource (regraph d wheel))
                        , Form.preset "Prism" (toSource (regraph d prism))
                        , Form.preset "K5" (toSource (regraph d complete))
                        ]
                    ]
                , Form.hint "Edit nodes and edges directly in the Code tab."
                ]


{-| Adopt a preset graph but keep the iteration count and colour the user already chose. -}
regraph : Model -> Model -> Model
regraph current shape =
    { shape | iterations = current.iterations, stroke = current.stroke }



-- PRESETS -----------------------------------------------------------------------------------------


wheel : Model
wheel =
    { nodes = [ "hub", "a", "b", "c", "d", "e", "f" ]
    , edges =
        [ ( 0, 1 ), ( 0, 2 ), ( 0, 3 ), ( 0, 4 ), ( 0, 5 ), ( 0, 6 )
        , ( 1, 2 ), ( 2, 3 ), ( 3, 4 ), ( 4, 5 ), ( 5, 6 ), ( 6, 1 )
        ]
    , iterations = 120
    , stroke = "#5fd0ff"
    }


prism : Model
prism =
    { nodes = [ "a", "b", "c", "d", "e", "f" ]
    , edges =
        [ ( 0, 1 ), ( 1, 2 ), ( 2, 0 )
        , ( 3, 4 ), ( 4, 5 ), ( 5, 3 )
        , ( 0, 3 ), ( 1, 4 ), ( 2, 5 )
        ]
    , iterations = 120
    , stroke = "#7cfc9b"
    }


complete : Model
complete =
    { nodes = [ "1", "2", "3", "4", "5" ]
    , edges =
        [ ( 0, 1 ), ( 0, 2 ), ( 0, 3 ), ( 0, 4 )
        , ( 1, 2 ), ( 1, 3 ), ( 1, 4 )
        , ( 2, 3 ), ( 2, 4 )
        , ( 3, 4 )
        ]
    , iterations = 120
    , stroke = "#ff9cee"
    }
