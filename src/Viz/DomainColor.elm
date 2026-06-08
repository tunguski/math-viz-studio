module Viz.DomainColor exposing (viz)

{-| **Domain colouring** — a way to picture a complex function `f(z)` of a complex variable, which
would otherwise need four dimensions. Each point `z` of the plane is painted with the hue of the
output `f(z)`'s argument (angle) and a lightness from its magnitude, so zeros, poles and the way the
function wraps the plane all show up as colour.
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
    { fn : String
    , resolution : Int
    , hue : Float
    }


viz : Viz
viz =
    { kind = "domain"
    , name = "Domain colouring"
    , description = "Picture a complex function by colour — hue is its angle."
    , about = "A function of a complex variable maps the plane to the plane, so its graph would need four dimensions to draw. Domain colouring solves this by painting the input plane: at each point z, the colour's hue is the angle (argument) of the output f(z), and the brightness comes from its magnitude.\n\nThe whole behaviour of the function then becomes visible at a glance. A zero is a point where all hues meet going one way round; a pole is where they meet going the other way; the number of times the colours cycle around a point reveals the order of a zero or pole. It is the standard modern tool for seeing complex analysis."
    , starter = toSource default
    , movable = False
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { fn = "z3m1", resolution = 120, hue = 0 }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map3 Model
                    (Value.strField "fn" fs)
                    (Value.intField "resolution" fs)
                    (Value.numField "hue" fs)
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"domain\"\n"
        ++ "    , fn = \""
        ++ d.fn
        ++ "\"\n    , resolution = "
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
            2.2

        s =
            580 / toFloat res

        w =
            Draw.r1 (s + 0.6)

        coord c =
            -extent + 2 * extent * toFloat c / toFloat (res - 1)

        rect i j =
            "M" ++ Draw.r1 (-290 + toFloat i * s) ++ " " ++ Draw.r1 (-290 + toFloat j * s) ++ "h" ++ w ++ "v" ++ w ++ "h-" ++ w ++ "z"

        cells =
            List.concatMap
                (\j -> List.map (\i -> ( bandOf (apply d.fn ( coord i, coord j )), rect i j )) (List.range 0 (res - 1)))
                (List.range 0 (res - 1))

        buckets =
            List.foldl (\( band, r ) acc -> Dict.update band (\m -> Just (r :: Maybe.withDefault [] m)) acc) Dict.empty cells
    in
    \mode phase ->
        let
            offset =
                if Color.timeVarying mode then
                    phase * 60

                else
                    0

            bandPath ( band, rects ) =
                Svg.path [ A.d (String.concat rects), A.fill (bandColor (d.hue + offset) band) ] []
        in
        Draw.stage (List.map bandPath (Dict.toList buckets))


{-| Quantise a complex value into a (hue band, lightness band) key. -}
bandOf : ( Float, Float ) -> Int
bandOf ( re, im ) =
    let
        arg =
            atan2 im re

        hueBand =
            modBy 36 (floor ((arg + pi) / (2 * pi) * 36))

        mag =
            sqrt (re * re + im * im)

        lBand =
            clamp 0 4 (floor (mag / (mag + 1) * 5))
    in
    hueBand * 5 + lBand


bandColor : Float -> Int -> String
bandColor hue band =
    let
        hueBand =
            band // 5

        lBand =
            modBy 5 band
    in
    "hsl(" ++ String.fromInt (modBy 360 (round hue + hueBand * 10)) ++ ", 80%, " ++ String.fromInt (22 + lBand * 12) ++ "%)"


{-| Apply the selected complex function. -}
apply : String -> ( Float, Float ) -> ( Float, Float )
apply fn z =
    case fn of
        "z2" ->
            mul z z

        "z3" ->
            mul (mul z z) z

        "z3m1" ->
            sub (mul (mul z z) z) ( 1, 0 )

        "z3mz" ->
            sub (mul (mul z z) z) z

        "inv" ->
            let
                ( x, y ) =
                    z

                dd =
                    x * x + y * y + 1.0e-9
            in
            ( x / dd, -y / dd )

        "sin" ->
            let
                ( x, y ) =
                    z
            in
            ( sin x * (e ^ y + e ^ -y) / 2, cos x * (e ^ y - e ^ -y) / 2 )

        "moebius" ->
            divide (sub z ( 1, 0 )) (add z ( 1, 0 ))

        _ ->
            z


mul : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
mul ( a, b ) ( c, d ) =
    ( a * c - b * d, a * d + b * c )


add : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
add ( a, b ) ( c, d ) =
    ( a + c, b + d )


sub : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
sub ( a, b ) ( c, d ) =
    ( a - c, b - d )


divide : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
divide ( a, b ) ( c, d ) =
    let
        dd =
            c * c + d * d + 1.0e-9
    in
    ( (a * c + b * d) / dd, (b * c - a * d) / dd )



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a domain colouring yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Function f(z)"
                    [ Form.presets
                        [ Form.preset "z²" (toSource { d | fn = "z2" })
                        , Form.preset "z³" (toSource { d | fn = "z3" })
                        , Form.preset "z³ − 1" (toSource { d | fn = "z3m1" })
                        , Form.preset "z³ − z" (toSource { d | fn = "z3mz" })
                        , Form.preset "1 / z" (toSource { d | fn = "inv" })
                        , Form.preset "sin z" (toSource { d | fn = "sin" })
                        , Form.preset "(z−1)/(z+1)" (toSource { d | fn = "moebius" })
                        ]
                    ]
                , Form.group "Display"
                    [ Form.slider "hue" 0 360 1 d.hue (\v -> toSource { d | hue = v })
                    , Form.slider "resolution" 40 160 2 (toFloat d.resolution) (\v -> toSource { d | resolution = round v })
                    ]
                , Form.hint "Zeros are where all hues meet; the wraps count the order."
                ]
