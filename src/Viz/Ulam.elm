module Viz.Ulam exposing (viz)

{-| **Ulam spiral** — write the integers `1, 2, 3, …` along a square spiral and mark the primes. They
fall, surprisingly, along diagonal lines (related to prime-rich quadratic polynomials). A `size × size`
grid; the marked primes are one filled `<path>`.
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
    { size : Int
    , stroke : String
    }


viz : Viz
viz =
    { kind = "ulam"
    , name = "Ulam spiral"
    , description = "Primes on a number spiral — they line up on diagonals."
    , about = "Bored during a talk in 1963, Stanisław Ulam idly wrote the whole numbers 1, 2, 3, … in a square spiral and circled the primes. To his surprise they were not scattered at random — many fell along diagonal lines.\n\nThose diagonals correspond to prime-rich quadratic polynomials, such as Euler's famous n² + n + 41, which yields primes for forty straight values of n. The Ulam spiral makes visible a faint, still-mysterious order in the distribution of the primes, and helped popularise computer experiments in number theory."
    , starter = toSource default
    , movable = False
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { size = 99, stroke = "#7dd3fc" }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map2 Model
                    (Value.intField "size" fs)
                    (Value.strField "stroke" fs)
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"ulam\"\n"
        ++ "    , size = "
        ++ String.fromInt d.size
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"



-- RENDER ------------------------------------------------------------------------------------------


prepare : Model -> (String -> Float -> Svg msg)
prepare d =
    let
        size =
            clamp 21 161 d.size

        cell =
            560 / toFloat size

        dotSize =
            cell * 0.62

        square ( _, x, y ) =
            let
                sx =
                    toFloat x * cell - dotSize / 2

                sy =
                    -(toFloat y) * cell - dotSize / 2
            in
            "M" ++ Draw.r1 sx ++ " " ++ Draw.r1 sy ++ "h" ++ Draw.r1 dotSize ++ "v" ++ Draw.r1 dotSize ++ "h" ++ Draw.r1 (-dotSize) ++ "z"

        path =
            String.concat
                (List.map square (List.filter (\( n, _, _ ) -> isPrime n) (walk (size * size))))
    in
    \mode phase ->
        Draw.stage [ Svg.path [ A.d path, A.fill (Color.solid (Color.resolve mode d.stroke) phase), A.opacity "0.95" ] [] ]


{-| The positions of `1 … total` along the square spiral, as `(n, x, y)`. -}
walk : Int -> List ( Int, Int, Int )
walk total =
    let
        dir di =
            case modBy 4 di of
                0 ->
                    ( 1, 0 )

                1 ->
                    ( 0, 1 )

                2 ->
                    ( -1, 0 )

                _ ->
                    ( 0, -1 )

        step n st =
            let
                out2 =
                    ( n, st.x, st.y ) :: st.out

                ( dx, dy ) =
                    dir st.di

                stepInLeg =
                    st.stepInLeg + 1
            in
            if stepInLeg >= st.legLen then
                let
                    legs =
                        st.legs + 1
                in
                { out = out2
                , x = st.x + dx
                , y = st.y + dy
                , di = st.di + 1
                , stepInLeg = 0
                , legs =
                    if legs >= 2 then
                        0

                    else
                        legs
                , legLen =
                    if legs >= 2 then
                        st.legLen + 1

                    else
                        st.legLen
                }

            else
                { st | out = out2, x = st.x + dx, y = st.y + dy, stepInLeg = stepInLeg }

        final =
            List.foldl step { x = 0, y = 0, di = 0, stepInLeg = 0, legs = 0, legLen = 1, out = [] } (List.range 1 total)
    in
    List.reverse final.out


isPrime : Int -> Bool
isPrime n =
    if n < 2 then
        False

    else if n < 4 then
        True

    else if modBy 2 n == 0 then
        False

    else
        let
            go k =
                if k * k > n then
                    True

                else if modBy k n == 0 then
                    False

                else
                    go (k + 2)
        in
        go 3



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't an Ulam spiral yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Spiral"
                    [ Form.slider "size" 21 161 2 (toFloat d.size) (\v -> toSource { d | size = round v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Small 51" (toSource { d | size = 51 })
                        , Form.preset "75" (toSource { d | size = 75 })
                        , Form.preset "99" (toSource { d | size = 99 })
                        , Form.preset "125" (toSource { d | size = 125 })
                        , Form.preset "Large 149" (toSource { d | size = 149 })
                        ]
                    ]
                ]
