module Viz.Voronoi exposing (viz)

{-| **Voronoi diagram** — given a set of `sites`, colour every point of the plane by which site is
nearest. The plane shatters into convex cells, one per site. The model *is* the list of sites.
-}

import Color
import Dict exposing (Dict)
import Draw
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as A
import Value
import Viz exposing (Viz)


type alias Model =
    { sites : List ( Float, Float )
    , resolution : Int
    , hue : Float
    }


viz : Viz
viz =
    { kind = "voronoi"
    , name = "Voronoi diagram"
    , description = "The plane carved into cells by nearest site."
    , about = "A Voronoi diagram takes a set of points — \"sites\" — and assigns every other point to its nearest site, carving the plane into convex cells. Named after Georgy Voronoy (1908), the idea was used decades earlier by John Snow, who mapped cholera deaths around a Soho water pump to trace a London epidemic.\n\nVoronoi cells are everywhere: modelling crystal grains and animal territories, planning where to put the nearest shop or cell tower, generating organic textures in graphics, and — via their dual, the Delaunay triangulation — meshing surfaces for simulation."
    , starter = toSource default
    , movable = False
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { sites = scatter, resolution = 130, hue = 200 }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map3 Model
                    (Value.listField "sites" fs |> Result.andThen (Value.traverse site))
                    (Value.intField "resolution" fs)
                    (Value.numField "hue" fs)
            )


site : Value.V -> Result String ( Float, Float )
site v =
    Value.tuple v
        |> Result.andThen
            (\xs ->
                case xs of
                    [ a, b ] ->
                        Result.map2 Tuple.pair (Value.num a) (Value.num b)

                    _ ->
                        Err "Expected a 2-tuple site like ( 0.3, -0.5 )"
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"voronoi\"\n    , sites =\n        "
        ++ Value.renderList "        " (List.map (\( x, y ) -> "( " ++ Value.numStr x ++ ", " ++ Value.numStr y ++ " )") d.sites)
        ++ "\n    , resolution = "
        ++ String.fromInt d.resolution
        ++ "\n    , hue = "
        ++ Value.numStr d.hue
        ++ "\n    }\n"



-- RENDER ------------------------------------------------------------------------------------------


prepare : Model -> (String -> Float -> Svg msg)
prepare d =
    let
        res =
            clamp 30 170 d.resolution

        extent =
            1.2

        s =
            580 / toFloat res

        w =
            Draw.r1 (s + 0.6)

        n =
            max 1 (List.length d.sites)

        indexed =
            List.indexedMap (\i ( x, y ) -> ( i, x, y )) d.sites

        nearest cx cy =
            List.foldl
                (\( i, sx, sy ) ( bi, bd ) ->
                    let
                        dd =
                            (cx - sx) * (cx - sx) + (cy - sy) * (cy - sy)
                    in
                    if dd < bd then
                        ( i, dd )

                    else
                        ( bi, bd )
                )
                ( 0, 1.0e9 )
                indexed
                |> Tuple.first

        coord c =
            -extent + 2 * extent * toFloat c / toFloat (res - 1)

        rect i j =
            "M" ++ Draw.r1 (-290 + toFloat i * s) ++ " " ++ Draw.r1 (-290 + toFloat j * s) ++ "h" ++ w ++ "v" ++ w ++ "h-" ++ w ++ "z"

        cells =
            List.concatMap
                (\j -> List.map (\i -> ( nearest (coord i) (coord j), rect i j )) (List.range 0 (res - 1)))
                (List.range 0 (res - 1))

        buckets =
            List.foldl (\( idx, r ) acc -> Dict.update idx (\m -> Just (r :: Maybe.withDefault [] m)) acc) Dict.empty cells

        dot ( x, y ) =
            Svg.circle [ A.cx (Draw.r2 (x / extent * 290)), A.cy (Draw.r2 (-y / extent * 290)), A.r "2.5", A.fill "#0b0e14" ] []
    in
    \mode phase ->
        let
            offset =
                if Color.timeVarying mode then
                    phase * 60

                else
                    0

            cell ( idx, rects ) =
                Svg.path
                    [ A.d (String.concat rects)
                    , A.fill ("hsl(" ++ String.fromInt (modBy 360 (round (d.hue + offset) + idx * 360 // n)) ++ ", 58%, 52%)")
                    , A.opacity "0.9"
                    ]
                    []
        in
        Draw.stage (List.map cell (Dict.toList buckets) ++ List.map dot d.sites)



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a Voronoi diagram yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Display"
                    [ Form.slider "hue" 0 360 1 d.hue (\v -> toSource { d | hue = v })
                    , Form.slider "resolution" 40 160 2 (toFloat d.resolution) (\v -> toSource { d | resolution = round v })
                    ]
                , Form.group "Sites"
                    [ Form.presets
                        [ Form.preset "Scatter" (toSource { d | sites = scatter })
                        , Form.preset "Few" (toSource { d | sites = List.take 5 scatter })
                        , Form.preset "Ring" (toSource { d | sites = ring })
                        , Form.preset "Grid" (toSource { d | sites = lattice })
                        , Form.preset "Pair" (toSource { d | sites = [ ( -0.5, 0 ), ( 0.5, 0 ) ] })
                        , Form.preset "Triangle" (toSource { d | sites = [ ( 0, 0.7 ), ( -0.6, -0.4 ), ( 0.6, -0.4 ) ] })
                        ]
                    ]
                , Form.hint "Edit the list of sites directly in the Code tab."
                ]



-- SITE SETS ---------------------------------------------------------------------------------------


scatter : List ( Float, Float )
scatter =
    [ ( -0.6, 0.5 ), ( 0.2, 0.7 ), ( 0.7, 0.3 ), ( -0.8, -0.2 ), ( 0.1, 0.05 ), ( 0.6, -0.5 ), ( -0.3, -0.6 ), ( 0.85, -0.1 ), ( -0.1, -0.85 ), ( 0.4, 0.35 ), ( -0.45, 0.15 ), ( 0.0, 0.45 ) ]


ring : List ( Float, Float )
ring =
    List.map (\k -> let a = 2 * pi * toFloat k / 9 in ( 0.7 * cos a, 0.7 * sin a )) (List.range 0 8)


lattice : List ( Float, Float )
lattice =
    List.concatMap (\i -> List.map (\j -> ( -0.7 + 0.7 * toFloat i, -0.7 + 0.7 * toFloat j )) (List.range 0 2)) (List.range 0 2)
