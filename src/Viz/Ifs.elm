module Viz.Ifs exposing (viz)

{-| **Iterated function system** — a set of affine maps, each chosen with probability `p` in a chaos
game that scatters `points` dots. The classic example is the Barnsley fern. Self-contained: model,
decode/print, render and controls all live here.
-}

import Color
import Draw
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Value exposing (V)
import Viz exposing (Viz)


type alias Model =
    { maps : List Affine
    , points : Int
    , stroke : String
    }


{-| One affine map `(x, y) ↦ (a·x + b·y + e, c·x + d·y + f)`, applied with probability `p`. -}
type alias Affine =
    { a : Float, b : Float, c : Float, d : Float, e : Float, f : Float, p : Float }


viz : Viz
viz =
    { kind = "ifs"
    , name = "Iterated function system"
    , description = "A chaos-game fractal, like the Barnsley fern."
    , starter = toSource fern
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
                Result.map3 Model
                    (Value.listField "maps" fs |> Result.andThen (Value.traverse affine))
                    (Value.intField "points" fs)
                    (Value.strField "stroke" fs)
            )


affine : V -> Result String Affine
affine v =
    Value.record v
        |> Result.andThen
            (\fs ->
                Result.map5 (\a b c d e -> ( a, b, ( c, d, e ) ))
                    (Value.numField "a" fs)
                    (Value.numField "b" fs)
                    (Value.numField "c" fs)
                    (Value.numField "d" fs)
                    (Value.numField "e" fs)
                    |> Result.andThen
                        (\( a, b, ( c, d, e ) ) ->
                            Result.map2 (\f p -> Affine a b c d e f p)
                                (Value.numField "f" fs)
                                (Value.numField "p" fs)
                        )
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"ifs\"\n"
        ++ "    , maps =\n        "
        ++ Value.renderList "        " (List.map affineStr d.maps)
        ++ "\n    , points = "
        ++ String.fromInt d.points
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"


affineStr : Affine -> String
affineStr m =
    let
        n =
            Value.numStr
    in
    "{ a = " ++ n m.a ++ ", b = " ++ n m.b ++ ", c = " ++ n m.c ++ ", d = " ++ n m.d ++ ", e = " ++ n m.e ++ ", f = " ++ n m.f ++ ", p = " ++ n m.p ++ " }"



-- RENDER ------------------------------------------------------------------------------------------


{-| Run the chaos game **once** into screen points; the returned drawer only recolours the cloud (and
under a gradient/pulse colouring, bands it across the figure). -}
prepare : Model -> (String -> Float -> Svg msg)
prepare d =
    let
        raw =
            chaosGame d.maps (clamp 100 40000 d.points)

        ( cx, cy, scale ) =
            Draw.fitTransform raw

        pts =
            List.map (\( x, y ) -> ( (x - cx) * scale, -(y - cy) * scale )) raw
    in
    \mode phase -> Draw.stage [ Draw.cloud "0.85" (Color.resolve mode d.stroke) phase pts ]


{-| The chaos game: from the origin, repeatedly pick a map (by its probability) and apply it,
collecting the visited points. Randomness is a small deterministic MINSTD generator seeded from a
constant, so the picture is stable frame to frame and needs no `elm/random`. -}
chaosGame : List Affine -> Int -> List ( Float, Float )
chaosGame maps count =
    let
        total =
            List.sum (List.map .p maps)

        cum =
            cumulative maps

        step _ ( seed, ( x, y ), acc ) =
            let
                seed2 =
                    modBy 2147483647 (48271 * seed)

                r =
                    toFloat seed2 / 2147483647 * total

                m =
                    Maybe.withDefault identityAffine (pick cum r)

                nx =
                    m.a * x + m.b * y + m.e

                ny =
                    m.c * x + m.d * y + m.f
            in
            ( seed2, ( nx, ny ), ( nx, ny ) :: acc )

        ( _, _, pts ) =
            List.foldl step ( 7919, ( 0, 0 ), [] ) (List.range 1 count)
    in
    List.drop 10 (List.reverse pts)


identityAffine : Affine
identityAffine =
    { a = 1, b = 0, c = 0, d = 1, e = 0, f = 0, p = 1 }


cumulative : List Affine -> List ( Float, Affine )
cumulative maps =
    let
        go acc sofar ms =
            case ms of
                m :: rest ->
                    go (( sofar + m.p, m ) :: acc) (sofar + m.p) rest

                [] ->
                    List.reverse acc
    in
    go [] 0 maps


pick : List ( Float, Affine ) -> Float -> Maybe Affine
pick cum r =
    case cum of
        ( thr, m ) :: rest ->
            if r <= thr || List.isEmpty rest then
                Just m

            else
                pick rest r

        [] ->
            Nothing



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't an iterated function system yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Cloud"
                    [ Form.slider "points" 1000 40000 500 (toFloat d.points) (\v -> toSource { d | points = round v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Barnsley fern" (toSource fern)
                        , Form.preset "Sierpiński" (toSource sierpinski)
                        , Form.preset "Twin dragon" (toSource dragon)
                        ]
                    ]
                , Form.group "Affine maps"
                    [ Form.table
                        (Form.tableHead [ "a", "b", "c", "d", "e", "f", "p" ]
                            :: List.indexedMap (affineRow d) d.maps
                        )
                    ]
                ]


affineRow : Model -> Int -> Affine -> Html String
affineRow d i m =
    let
        cell get put =
            Form.numCell (get m) (\v -> toSource { d | maps = Form.updateAt i (put v) d.maps })
    in
    Form.row
        [ cell .a (\v q -> { q | a = v })
        , cell .b (\v q -> { q | b = v })
        , cell .c (\v q -> { q | c = v })
        , cell .d (\v q -> { q | d = v })
        , cell .e (\v q -> { q | e = v })
        , cell .f (\v q -> { q | f = v })
        , cell .p (\v q -> { q | p = v })
        ]



-- PRESETS -----------------------------------------------------------------------------------------


fern : Model
fern =
    { maps =
        [ { a = 0, b = 0, c = 0, d = 0.16, e = 0, f = 0, p = 0.01 }
        , { a = 0.85, b = 0.04, c = -0.04, d = 0.85, e = 0, f = 1.6, p = 0.85 }
        , { a = 0.2, b = -0.26, c = 0.23, d = 0.22, e = 0, f = 1.6, p = 0.07 }
        , { a = -0.15, b = 0.28, c = 0.26, d = 0.24, e = 0, f = 0.44, p = 0.07 }
        ]
    , points = 14000
    , stroke = "#7cfc9b"
    }


sierpinski : Model
sierpinski =
    { maps =
        [ { a = 0.5, b = 0, c = 0, d = 0.5, e = 0, f = 0, p = 0.34 }
        , { a = 0.5, b = 0, c = 0, d = 0.5, e = 1, f = 0, p = 0.33 }
        , { a = 0.5, b = 0, c = 0, d = 0.5, e = 0.5, f = 0.87, p = 0.33 }
        ]
    , points = 14000
    , stroke = "#ffd479"
    }


dragon : Model
dragon =
    { maps =
        [ { a = 0.5, b = -0.5, c = 0.5, d = 0.5, e = 0, f = 0, p = 0.5 }
        , { a = -0.5, b = -0.5, c = 0.5, d = -0.5, e = 1, f = 0, p = 0.5 }
        ]
    , points = 16000
    , stroke = "#ff9cee"
    }
