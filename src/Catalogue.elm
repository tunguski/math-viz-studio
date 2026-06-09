module Catalogue exposing (main)

{-| A **static reference page** built from Elm at build time. It walks the same `registry` the studio
uses and renders one section per visualisation, each with a live sample (the starter rendered at a
fixed colour, no animation), an SVG of the defining formula, and the description from the module's
`about` text. Compiled to its own `catalogue.js`/`catalogue.html` by `build.sh`.
-}

import Browser
import Dict exposing (Dict)
import Html exposing (Html, a, div, h1, h2, p, text)
import Html.Attributes exposing (class, href, id)
import Main exposing (registry)
import Svg
import Svg.Attributes as A
import Viz exposing (Viz)


main : Program () () Never
main =
    Browser.element
        { init = \_ -> ( (), Cmd.none )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , view = \_ -> page
        }


page : Html msg
page =
    div [ class "cat" ]
        [ div [ class "cat-head" ]
            [ h1 [ class "cat-title" ] [ text "MathViz Studio — Catalogue" ]
            , p [ class "cat-sub" ]
                [ text (String.fromInt (List.length registry) ++ " mathematical visualisations — each a plain Elm data structure, rendered live. ")
                , a [ href "index.html", class "cat-link" ] [ text "Open the studio →" ]
                ]
            , div [ class "cat-toc" ] (List.map tocChip registry)
            ]
        , div [ class "cat-list" ] (List.map section registry)
        , div [ class "cat-foot" ] [ text "Built with elm-lang — the whole page is generated from Elm." ]
        ]


tocChip : Viz -> Html msg
tocChip v =
    a [ href ("#" ++ v.kind), class "cat-chip" ] [ text v.name ]


section : Viz -> Html msg
section v =
    div [ class "cat-sec", id v.kind ]
        [ div [ class "cat-card" ]
            [ div [ class "cat-sample" ] [ div [ class "cat-stage" ] [ sample v ] ]
            , div [ class "cat-info" ]
                (h2 [ class "cat-name" ] [ text v.name ]
                    :: div [ class "cat-tag" ] [ text v.description ]
                    :: div [ class "cat-formula" ] [ formulaSvg (formulaFor v) ]
                    :: List.map (\para -> p [ class "cat-p" ] [ text para ]) (paragraphs v.about)
                )
            ]
        ]


{-| The canonical sample picture — the starter, fixed colour, phase 0. -}
sample : Viz -> Html msg
sample v =
    case v.render v.starter of
        Ok drawer ->
            Html.map never (drawer "fixed" 0)

        Err _ ->
            text ""


paragraphs : String -> List String
paragraphs s =
    List.filter (\para -> para /= "") (String.split "\n\n" s)


{-| Lay the formula out as SVG text (one `<text>` per line); the viewBox is sized to the longest line
so nothing clips and it scales to the column width. Styling (italic serif, colour) is in the host
page's CSS, targeting `.cat-formula text`. -}
formulaSvg : String -> Html msg
formulaSvg f =
    let
        lines =
            String.split "\n" f

        longest =
            List.foldl (\l m -> max m (String.length l)) 1 lines

        w =
            max 260 (longest * 11)

        h =
            List.length lines * 30 + 18

        row i l =
            Svg.text_ [ A.x "16", A.y (String.fromInt (32 + i * 30)) ] [ Svg.text l ]
    in
    Svg.svg
        [ A.viewBox ("0 0 " ++ String.fromInt w ++ " " ++ String.fromInt h)
        , A.width "100%"
        , A.height (String.fromInt h)
        , A.preserveAspectRatio "xMinYMid meet"
        ]
        (List.indexedMap row lines)


formulaFor : Viz -> String
formulaFor v =
    Maybe.withDefault v.description (Dict.get v.kind formulas)


{-| The defining formula for each visualisation, keyed by `kind`. Written in plain Unicode maths so it
renders as SVG text without a typesetting engine; lines split on `\n`. -}
formulas : Dict String String
formulas =
    Dict.fromList
        [ ( "harmonograph", "x(t) = Σᵢ Aᵢ · e^(−dᵢ t) · sin(fᵢ t + φᵢ)\ny(t) = Σⱼ Aⱼ · e^(−dⱼ t) · sin(fⱼ t + φⱼ)" )
        , ( "rose", "r = sin(n · θ),    θ = k · d°,    k = 0, 1, 2, …" )
        , ( "lissajous", "x = sin(a · t + δ),    y = sin(b · t)" )
        , ( "spirograph", "x = (R − r)·cos t + ρ·cos( (R−r)/r · t )\ny = (R − r)·sin t − ρ·sin( (R−r)/r · t )" )
        , ( "epicycloid", "x = (R + r)·cos t − r·cos( (R+r)/r · t )\ny = (R + r)·sin t − r·sin( (R+r)/r · t )" )
        , ( "superformula", "r(θ) = ( |cos(mθ/4)/a|^n₂ + |sin(mθ/4)/b|^n₃ )^(−1/n₁)" )
        , ( "ifs", "(x, y) ↦ (a·x + b·y + e,   c·x + d·y + f)    with prob. p" )
        , ( "julia", "zₙ₊₁ = zₙ² + c" )
        , ( "mandelbrot", "zₙ₊₁ = zₙ² + c,    z₀ = 0    (bounded ⇔ c ∈ M)" )
        , ( "newton", "zₙ₊₁ = zₙ − (zₙ³ − 1) / (3 zₙ²)" )
        , ( "domain", "z ↦ f(z),    hue = arg f(z),    value = |f(z)|" )
        , ( "lsystem", "axiom → rewrite each symbol by its rule → draw with a turtle" )
        , ( "hilbert", "A → + B F − A F A − F B +\nB → − A F + B F B + F A −" )
        , ( "polyhedron", "p ↦ R_yaw · R_pitch · p,    then project to the plane" )
        , ( "torus", "x = (R + r·cos v)·cos u\ny = (R + r·cos v)·sin u\nz = r·sin v" )
        , ( "sphere", "r(u, v) = 1 + b · sin(l · v) · cos(m · u)" )
        , ( "mobius", "x = (1 + ½v·cos ½u)·cos u\ny = (1 + ½v·cos ½u)·sin u\nz = ½v·sin ½u" )
        , ( "klein", "x = (r + cos ½u·sin v − sin ½u·sin 2v)·cos u\ny = (r + cos ½u·sin v − sin ½u·sin 2v)·sin u\nz = sin ½u·sin v + cos ½u·sin 2v" )
        , ( "heightfield", "z = f(x, y)" )
        , ( "knot", "x = (2 + cos q t)·cos p t\ny = (2 + cos q t)·sin p t\nz = sin q t" )
        , ( "lorenz", "ẋ = σ(y − x)\nẏ = x(ρ − z) − y\nż = x·y − β·z" )
        , ( "rossler", "ẋ = −y − z\nẏ = x + a·y\nż = b + z(x − c)" )
        , ( "clifford", "xₙ₊₁ = sin(a·yₙ) + c·cos(a·xₙ)\nyₙ₊₁ = sin(b·xₙ) + d·cos(b·yₙ)" )
        , ( "dejong", "xₙ₊₁ = sin(a·yₙ) − cos(b·xₙ)\nyₙ₊₁ = sin(c·xₙ) − cos(d·yₙ)" )
        , ( "henon", "xₙ₊₁ = 1 − a·xₙ² + yₙ\nyₙ₊₁ = b·xₙ" )
        , ( "bifurcation", "xₙ₊₁ = r · xₙ · (1 − xₙ)" )
        , ( "field", "(ẋ, ẏ) = A · (x, y),    A = [[a, b], [c, d]]" )
        , ( "matrix", "(x, y) ↦ A · (x, y),    A = [[a, b], [c, d]]" )
        , ( "svd", "A = U · Σ · Vᵀ,    Σ = diag(σ₁, σ₂)" )
        , ( "eigen", "A · v = λ · v" )
        , ( "polynomials", "pᵢ(x) = Σₖ aₖ · xᵏ;    crossings solve  pᵢ(x) = pⱼ(x)" )
        , ( "phyllotaxis", "θₙ = n · 137.5°,    rₙ = c · √n" )
        , ( "walk", "pₙ₊₁ = pₙ + s · (cos θₙ, sin θₙ),    θₙ uniform" )
        , ( "levy", "step ℓ ~ P(ℓ) ∝ ℓ^(−1−α),    direction uniform" )
        , ( "ulam", "spiral 1, 2, 3, … over ℤ²;   mark n where n is prime" )
        , ( "voronoi", "cell(sᵢ) = { p : |p − sᵢ| ≤ |p − sⱼ| for all j }" )
        , ( "truchet", "each tile ∈ { ◜◞ , ◝◟ },   chosen at random" )
        , ( "ford", "C(p/q):   centre (p/q, 1/2q²),   radius 1/2q²" )
        , ( "hull", "H = smallest convex set ⊇ { p₁, …, pₙ }" )
        , ( "chladni", "sin(nπx)·sin(mπy) = sin(mπx)·sin(nπy)" )
        , ( "graph", "minimise   Σ spring(edges) + Σ repulsion(node pairs)" )
        , ( "automaton", "cᵢⁿ⁺¹ = R( cᵢ₋₁ⁿ, cᵢⁿ, cᵢ₊₁ⁿ )" )
        ]
