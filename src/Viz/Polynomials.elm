module Viz.Polynomials exposing (viz)

{-| **Polynomial intersections** — define any number of polynomials (each just a list of
coefficients) and see them plotted together, with every pairwise intersection marked and listed.
Where two curves meet is exactly where the system `pᵢ(x) = pⱼ(x)` is solved; the crossing points are
the real roots of their difference, found here by scanning for sign changes and bisecting.

The model *is* the list of polynomials — edit the coefficients directly in the Code tab.
-}

import Draw
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as A
import Value
import Viz exposing (Viz)


type alias Model =
    { polys : List Poly
    , xMin : Float
    , xMax : Float
    , yMin : Float
    , yMax : Float
    }


{-| One polynomial: `coeffs = [ a₀, a₁, a₂, … ]` means `a₀ + a₁·x + a₂·x² + …`. -}
type alias Poly =
    { coeffs : List Float
    , color : String
    }


{-| A computed crossing between polynomials `i` and `j` at `(x, y)`. -}
type alias Hit =
    { i : Int, j : Int, x : Float, y : Float }


viz : Viz
viz =
    { kind = "polynomials"
    , name = "Polynomial intersections"
    , description = "Plot several polynomials and list where they cross."
    , about = "Two curves cross exactly where the equation pᵢ(x) = pⱼ(x) holds — so finding the intersections of a set of polynomials is the same as solving the systems formed by every pair. For straight lines that is a linear system, and the crossing is found by elimination; for higher degrees the crossings are the real roots of the difference polynomial pᵢ − pⱼ.\n\nThis view plots each polynomial (a plain list of coefficients in the model) and marks every pairwise crossing inside the window, scanning for sign changes and bisecting to locate each root. The coordinates are listed both on the plot and in the panel — a small geometry of simultaneous equations."
    , starter = toSource default
    , movable = False
    , render = \source -> Result.map prepare (decode source)
    , controls = controls
    }


default : Model
default =
    { polys =
        [ { coeffs = [ 0, 1 ], color = "#5fd0ff" }
        , { coeffs = [ -2, 0, 1 ], color = "#fbbf24" }
        , { coeffs = [ 0, -1, 0, 0.5 ], color = "#ff9cee" }
        ]
    , xMin = -3
    , xMax = 3
    , yMin = -4
    , yMax = 4
    }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map5 Model
                    (Value.listField "polys" fs |> Result.andThen (Value.traverse poly))
                    (Value.numField "xMin" fs)
                    (Value.numField "xMax" fs)
                    (Value.numField "yMin" fs)
                    (Value.numField "yMax" fs)
            )


poly : Value.V -> Result String Poly
poly v =
    Value.record v
        |> Result.andThen
            (\fs ->
                Result.map2 Poly
                    (Value.listField "coeffs" fs |> Result.andThen (Value.traverse Value.num))
                    (Value.strField "color" fs)
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"polynomials\"\n    , polys =\n        "
        ++ Value.renderList "        " (List.map polyStr d.polys)
        ++ "\n    , xMin = "
        ++ Value.numStr d.xMin
        ++ "\n    , xMax = "
        ++ Value.numStr d.xMax
        ++ "\n    , yMin = "
        ++ Value.numStr d.yMin
        ++ "\n    , yMax = "
        ++ Value.numStr d.yMax
        ++ "\n    }\n"


polyStr : Poly -> String
polyStr p =
    "{ coeffs = [ " ++ String.join ", " (List.map Value.numStr p.coeffs) ++ " ], color = \"" ++ p.color ++ "\" }"



-- MATHS -------------------------------------------------------------------------------------------


{-| Evaluate `a₀ + a₁·x + a₂·x² + …` by Horner's rule. -}
eval : List Float -> Float -> Float
eval coeffs x =
    List.foldr (\c acc -> c + x * acc) 0 coeffs


{-| All pairwise crossings inside the x-window. -}
intersectionsOf : Model -> List Hit
intersectionsOf d =
    let
        indexed =
            List.indexedMap Tuple.pair d.polys
    in
    List.concatMap
        (\( ( i, pi_ ), ( j, pj ) ) ->
            findRoots (\x -> eval pi_.coeffs x - eval pj.coeffs x) d.xMin d.xMax
                |> List.map (\x -> { i = i, j = j, x = x, y = eval pi_.coeffs x })
        )
        (pairs indexed)


pairs : List a -> List ( a, a )
pairs xs =
    case xs of
        x :: rest ->
            List.map (\y -> ( x, y )) rest ++ pairs rest

        [] ->
            []


{-| Real roots of `f` in `[lo, hi]` by scanning for sign changes and bisecting each. -}
findRoots : (Float -> Float) -> Float -> Float -> List Float
findRoots f lo hi =
    let
        n =
            1200

        sx k =
            lo + (hi - lo) * toFloat k / toFloat n

        check k acc =
            let
                x0 =
                    sx k

                x1 =
                    sx (k + 1)

                y0 =
                    f x0

                y1 =
                    f x1
            in
            if y0 == 0 then
                x0 :: acc

            else if y0 * y1 < 0 then
                bisect f x0 x1 :: acc

            else
                acc
    in
    List.foldr check [] (List.range 0 (n - 1))


bisect : (Float -> Float) -> Float -> Float -> Float
bisect f lo hi =
    let
        go a b i =
            if i <= 0 then
                (a + b) / 2

            else
                let
                    m =
                        (a + b) / 2
                in
                if f a * f m <= 0 then
                    go a m (i - 1)

                else
                    go m b (i - 1)
    in
    go lo hi 60



-- RENDER ------------------------------------------------------------------------------------------


prepare : Model -> (String -> Float -> Svg msg)
prepare d =
    let
        xMin =
            d.xMin

        xMax =
            if d.xMax > d.xMin then
                d.xMax

            else
                d.xMin + 1

        yMin =
            d.yMin

        yMax =
            if d.yMax > d.yMin then
                d.yMax

            else
                d.yMin + 1

        sx x =
            (x - xMin) / (xMax - xMin) * 560 - 280

        sy y =
            clamp -1600 1600 (280 - (y - yMin) / (yMax - yMin) * 560)

        hits =
            intersectionsOf d

        curve p =
            let
                pts =
                    List.map
                        (\k ->
                            let
                                x =
                                    xMin + (xMax - xMin) * toFloat k / 360
                            in
                            Draw.r1 (sx x) ++ "," ++ Draw.r1 (sy (eval p.coeffs x))
                        )
                        (List.range 0 360)
            in
            Svg.polyline [ A.points (String.join " " pts), A.fill "none", A.stroke p.color, A.strokeWidth "2", A.strokeLinejoin "round" ] []

        dot h =
            Svg.circle [ A.cx (Draw.r1 (sx h.x)), A.cy (Draw.r1 (sy h.y)), A.r "3.4", A.fill "#f4f7fb", A.stroke "#0b0e14", A.strokeWidth "1" ] []

        inWindow h =
            h.y >= yMin && h.y <= yMax
    in
    \_ _ ->
        Draw.stage
            (axes sx sy xMin xMax yMin yMax
                ++ List.map curve d.polys
                ++ List.map dot (List.filter inWindow hits)
                ++ legend hits
            )


axes : (Float -> Float) -> (Float -> Float) -> Float -> Float -> Float -> Float -> List (Svg msg)
axes sx sy xMin xMax yMin yMax =
    let
        line x1 y1 x2 y2 =
            Svg.line [ A.x1 (Draw.r1 x1), A.y1 (Draw.r1 y1), A.x2 (Draw.r1 x2), A.y2 (Draw.r1 y2), A.stroke "#33405a", A.strokeWidth "1" ] []

        xAxis =
            if yMin <= 0 && yMax >= 0 then
                [ line (sx xMin) (sy 0) (sx xMax) (sy 0) ]

            else
                []

        yAxis =
            if xMin <= 0 && xMax >= 0 then
                [ line (sx 0) (sy yMin) (sx 0) (sy yMax) ]

            else
                []
    in
    xAxis ++ yAxis


{-| A text panel listing the crossings, drawn into the result pane. -}
legend : List Hit -> List (Svg msg)
legend hits =
    let
        shown =
            List.take 16 hits

        extra =
            List.length hits - List.length shown

        line k h =
            Svg.text_
                [ A.x "-286", A.y (String.fromInt (-276 + k * 15)), A.fontSize "11", A.fontFamily "ui-monospace, monospace", A.fill "#b6c4d6" ]
                [ Svg.text ("p" ++ String.fromInt (h.i + 1) ++ "∩p" ++ String.fromInt (h.j + 1) ++ "  (" ++ f2 h.x ++ ", " ++ f2 h.y ++ ")") ]

        more =
            if extra > 0 then
                [ Svg.text_
                    [ A.x "-286", A.y (String.fromInt (-276 + List.length shown * 15)), A.fontSize "11", A.fontFamily "ui-monospace, monospace", A.fill "#6b7a8d" ]
                    [ Svg.text ("+" ++ String.fromInt extra ++ " more") ]
                ]

            else
                []
    in
    if List.isEmpty hits then
        [ Svg.text_ [ A.x "-286", A.y "-278", A.fontSize "11", A.fontFamily "ui-monospace, monospace", A.fill "#6b7a8d" ] [ Svg.text "no intersections in window" ] ]

    else
        Svg.rect
            [ A.x "-296", A.y "-294", A.width "172", A.height (String.fromInt (22 + (List.length shown + min 1 extra) * 15)), A.rx "6", A.fill "#0b1018", A.opacity "0.66" ]
            []
            :: Svg.text_ [ A.x "-286", A.y "-294", A.fontSize "11", A.fontFamily "ui-monospace, monospace", A.fill "#8aa0b2" ] [ Svg.text "intersections" ]
            :: List.indexedMap line shown
            ++ more


f2 : Float -> String
f2 v =
    let
        r =
            toFloat (round (v * 100)) / 100
    in
    String.fromFloat
        (if r == 0 then
            0

         else
            r
        )



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a set of polynomials yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Window"
                    [ Form.slider "x min" -20 0 0.5 d.xMin (\v -> toSource { d | xMin = v })
                    , Form.slider "x max" 0 20 0.5 d.xMax (\v -> toSource { d | xMax = v })
                    , Form.slider "y min" -20 0 0.5 d.yMin (\v -> toSource { d | yMin = v })
                    , Form.slider "y max" 0 20 0.5 d.yMax (\v -> toSource { d | yMax = v })
                    ]
                , Form.group "Polynomials"
                    (List.indexedMap
                        (\i p -> Form.colorRow ("p" ++ String.fromInt (i + 1) ++ ":  " ++ formula p.coeffs) p.color (\c -> toSource { d | polys = Form.updateAt i (\q -> { q | color = c }) d.polys }))
                        d.polys
                    )
                , Form.group "Presets"
                    [ Form.presets
                        [ Form.preset "Line·parabola·cubic" (toSource default)
                        , Form.preset "Two parabolas" (toSource twoParabolas)
                        , Form.preset "Quartic & line" (toSource quarticLine)
                        , Form.preset "Pencil of lines" (toSource pencil)
                        , Form.preset "Cubic & parabola" (toSource cubicParabola)
                        ]
                    ]
                , intersectionsPanel d
                , Form.hint "Each polynomial is a list of coefficients [ a₀, a₁, a₂, … ] — add or edit them in the Code tab."
                ]


intersectionsPanel : Model -> Html String
intersectionsPanel d =
    let
        hits =
            intersectionsOf d

        rowFor h =
            Form.hint ("p" ++ String.fromInt (h.i + 1) ++ " ∩ p" ++ String.fromInt (h.j + 1) ++ "   ( " ++ f2 h.x ++ " ,  " ++ f2 h.y ++ " )")
    in
    Form.group ("Intersections (" ++ String.fromInt (List.length hits) ++ ")")
        (if List.isEmpty hits then
            [ Form.hint "None inside the current window." ]

         else
            List.map rowFor hits
        )


{-| Pretty-print a coefficient list as a polynomial, e.g. `[ -2, 0, 1 ]` → `x² − 2`. -}
formula : List Float -> String
formula coeffs =
    let
        terms =
            List.indexedMap Tuple.pair coeffs
                |> List.filter (\( _, c ) -> c /= 0)
                |> List.reverse

        piece ( k, c ) =
            (if c < 0 then
                "− "

             else
                "+ "
            )
                ++ coefStr (abs c) k
                ++ powStr k

        s =
            String.join " " (List.map piece terms)
    in
    if s == "" then
        "0"

    else if String.left 2 s == "+ " then
        String.dropLeft 2 s

    else if String.left 2 s == "− " then
        "−" ++ String.dropLeft 2 s

    else
        s


coefStr : Float -> Int -> String
coefStr mag k =
    if mag == 1 && k > 0 then
        ""

    else
        String.fromFloat mag


powStr : Int -> String
powStr k =
    case k of
        0 ->
            ""

        1 ->
            "x"

        2 ->
            "x²"

        3 ->
            "x³"

        4 ->
            "x⁴"

        5 ->
            "x⁵"

        _ ->
            "x^" ++ String.fromInt k



-- PRESETS -----------------------------------------------------------------------------------------


twoParabolas : Model
twoParabolas =
    { polys =
        [ { coeffs = [ -1, 0, 1 ], color = "#5fd0ff" }
        , { coeffs = [ 1, 0, -1 ], color = "#fbbf24" }
        ]
    , xMin = -3
    , xMax = 3
    , yMin = -3
    , yMax = 3
    }


quarticLine : Model
quarticLine =
    { polys =
        [ { coeffs = [ -2, 0, -1, 0, 0.3 ], color = "#a78bfa" }
        , { coeffs = [ 0, 0.5 ], color = "#7cfc9b" }
        ]
    , xMin = -3.5
    , xMax = 3.5
    , yMin = -4
    , yMax = 5
    }


pencil : Model
pencil =
    { polys =
        [ { coeffs = [ 1, 1 ], color = "#5fd0ff" }
        , { coeffs = [ 1, -1 ], color = "#fbbf24" }
        , { coeffs = [ -1, 2 ], color = "#ff9cee" }
        , { coeffs = [ 0.5, 0.2 ], color = "#7cfc9b" }
        ]
    , xMin = -4
    , xMax = 4
    , yMin = -4
    , yMax = 4
    }


cubicParabola : Model
cubicParabola =
    { polys =
        [ { coeffs = [ 0, -3, 0, 1 ], color = "#ff9cee" }
        , { coeffs = [ -1, 0, 1 ], color = "#fbbf24" }
        ]
    , xMin = -3
    , xMax = 3
    , yMin = -5
    , yMax = 5
    }
