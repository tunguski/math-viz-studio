module Controls exposing (build, gallery)

{-| The visual builder — the structured panels the shell plugs into the code pane next to the plain
text editor.

Both panels are pure views of the source that **emit a new source string** on every change; the shell
folds that back into the edited file (exactly as if it had been typed), so the sliders, the code and
the live picture stay three views of one model.

  - `build` parses the current scene and shows kind-specific controls — sliders, colour pickers,
    affine-map cells, shape presets. Dragging a slider re-prints the whole scene with the changed
    field, so the visualisation moves as you drag.
  - `gallery` is a chooser: each card emits a different starter scene, switching the whole
    visualisation (and its Elm data structure) to another kind.

-}

import Html exposing (Html, button, div, input, span, text)
import Html.Attributes as Attr exposing (class)
import Html.Events exposing (onClick, onInput)
import Scene
    exposing
        ( Affine
        , AutomatonData
        , GraphData
        , HarmonographData
        , IfsData
        , LorenzData
        , Pendulum
        , PolyhedronData
        , Scene(..)
        )


{-| The "Build" panel: kind-specific controls for the current scene. -}
build : Int -> String -> Html String
build _ source =
    case Scene.parse source of
        Ok (Harmonograph d) ->
            buildHarmonograph d

        Ok (Ifs d) ->
            buildIfs d

        Ok (Polyhedron d) ->
            buildPolyhedron d

        Ok (Lorenz d) ->
            buildLorenz d

        Ok (Graph d) ->
            buildGraph d

        Ok (Automaton d) ->
            buildAutomaton d

        Err e ->
            div [ class "mv-controls" ]
                [ div [ class "mv-error" ] [ text ("This file isn't a scene yet — " ++ e) ] ]


{-| The "Gallery" panel: pick a visualisation to switch to. -}
gallery : Int -> String -> Html String
gallery _ _ =
    div [ class "mv-gallery" ]
        [ galleryCard "Harmonograph" "Sums of damped sinusoids — looping plane curves." Scene.harmonographStarter
        , galleryCard "Iterated function system" "A chaos-game fractal, like the Barnsley fern." Scene.ifsStarter
        , galleryCard "Polyhedron" "A rotating wireframe solid you can spin." Scene.polyhedronStarter
        , galleryCard "Lorenz attractor" "A strange attractor — the chaotic butterfly flow." Scene.lorenzStarter
        , galleryCard "Force-directed graph" "A network laid out by a spring simulation." Scene.graphStarter
        , galleryCard "Cellular automaton" "An elementary Wolfram rule, drawn row by row." Scene.automatonStarter
        ]


galleryCard : String -> String -> String -> Html String
galleryCard title desc src =
    button [ class "mv-card", onClick src ]
        [ span [ class "mv-card-title" ] [ text title ]
        , span [ class "mv-card-desc" ] [ text desc ]
        ]



-- HARMONOGRAPH ------------------------------------------------------------------------------------


buildHarmonograph : HarmonographData -> Html String
buildHarmonograph d =
    div [ class "mv-controls" ]
        [ group "Trace"
            [ slider "samples" 200 12000 100 (toFloat d.samples) (\v -> Scene.toSource (Harmonograph { d | samples = round v }))
            , colorRow "stroke" d.stroke (\s -> Scene.toSource (Harmonograph { d | stroke = s }))
            ]
        , oscGroup "X oscillators" (\xs -> Harmonograph { d | x = xs }) d.x
        , oscGroup "Y oscillators" (\ys -> Harmonograph { d | y = ys }) d.y
        ]


oscGroup : String -> (List Pendulum -> Scene) -> List Pendulum -> Html String
oscGroup title setList ps =
    group title (List.concat (List.indexedMap (pendulumRow setList ps) ps))


pendulumRow : (List Pendulum -> Scene) -> List Pendulum -> Int -> Pendulum -> List (Html String)
pendulumRow setList ps i p =
    let
        on : (Float -> Pendulum -> Pendulum) -> Float -> String
        on f v =
            Scene.toSource (setList (updateAt i (f v) ps))
    in
    [ div [ class "mv-subhead" ] [ text ("oscillator " ++ String.fromInt (i + 1)) ]
    , slider "amplitude" 0 220 1 p.amp (on (\v q -> { q | amp = v }))
    , slider "frequency" 0.5 6 0.01 p.freq (on (\v q -> { q | freq = v }))
    , slider "phase" 0 6.2832 0.01 p.phase (on (\v q -> { q | phase = v }))
    , slider "decay" 0 0.02 0.0005 p.decay (on (\v q -> { q | decay = v }))
    ]



-- ITERATED FUNCTION SYSTEM ------------------------------------------------------------------------


buildIfs : IfsData -> Html String
buildIfs d =
    div [ class "mv-controls" ]
        [ group "Cloud"
            [ slider "points" 1000 40000 500 (toFloat d.points) (\v -> Scene.toSource (Ifs { d | points = round v }))
            , colorRow "stroke" d.stroke (\s -> Scene.toSource (Ifs { d | stroke = s }))
            ]
        , group "Presets"
            [ div [ class "mv-presets" ]
                [ preset "Barnsley fern" (Scene.toSource (Ifs Scene.ifsFern))
                , preset "Sierpiński" (Scene.toSource (Ifs Scene.ifsSierpinski))
                , preset "Twin dragon" (Scene.toSource (Ifs Scene.ifsDragon))
                ]
            ]
        , group "Affine maps"
            [ div [ class "mv-table" ]
                (tableHead [ "a", "b", "c", "d", "e", "f", "p" ]
                    :: List.indexedMap (affineRow d) d.maps
                )
            ]
        ]


affineRow : IfsData -> Int -> Affine -> Html String
affineRow d i m =
    let
        cell get set =
            numCell (get m) (\v -> Scene.toSource (Ifs { d | maps = updateAt i (set v) d.maps }))
    in
    div [ class "mv-trow" ]
        [ cell .a (\v q -> { q | a = v })
        , cell .b (\v q -> { q | b = v })
        , cell .c (\v q -> { q | c = v })
        , cell .d (\v q -> { q | d = v })
        , cell .e (\v q -> { q | e = v })
        , cell .f (\v q -> { q | f = v })
        , cell .p (\v q -> { q | p = v })
        ]



-- POLYHEDRON --------------------------------------------------------------------------------------


buildPolyhedron : PolyhedronData -> Html String
buildPolyhedron d =
    div [ class "mv-controls" ]
        [ group "Orientation"
            [ slider "yaw" 0 6.2832 0.01 d.yaw (\v -> Scene.toSource (Polyhedron { d | yaw = v }))
            , slider "pitch" 0 6.2832 0.01 d.pitch (\v -> Scene.toSource (Polyhedron { d | pitch = v }))
            , colorRow "stroke" d.stroke (\s -> Scene.toSource (Polyhedron { d | stroke = s }))
            ]
        , group "Shape"
            [ div [ class "mv-presets" ]
                [ preset "Cube" (Scene.toSource (Polyhedron (reshape d Scene.cube)))
                , preset "Tetrahedron" (Scene.toSource (Polyhedron (reshape d Scene.tetrahedron)))
                , preset "Octahedron" (Scene.toSource (Polyhedron (reshape d Scene.octahedron)))
                ]
            ]
        , div [ class "mv-hint" ] [ text "Edit vertices and edges directly in the Code tab." ]
        ]


{-| Adopt a preset's geometry but keep the orientation and colour the user already chose. -}
reshape : PolyhedronData -> PolyhedronData -> PolyhedronData
reshape current shape =
    { shape | yaw = current.yaw, pitch = current.pitch, stroke = current.stroke }



-- LORENZ ATTRACTOR --------------------------------------------------------------------------------


buildLorenz : LorenzData -> Html String
buildLorenz d =
    div [ class "mv-controls" ]
        [ group "Flow"
            [ slider "sigma (σ)" 0 20 0.1 d.sigma (\v -> Scene.toSource (Lorenz { d | sigma = v }))
            , slider "rho (ρ)" 0 100 0.1 d.rho (\v -> Scene.toSource (Lorenz { d | rho = v }))
            , slider "beta (β)" 0 5 0.01 d.beta (\v -> Scene.toSource (Lorenz { d | beta = v }))
            ]
        , group "Integration"
            [ slider "dt" 0.001 0.02 0.001 d.dt (\v -> Scene.toSource (Lorenz { d | dt = v }))
            , slider "steps" 1000 16000 500 (toFloat d.steps) (\v -> Scene.toSource (Lorenz { d | steps = round v }))
            ]
        , group "View"
            [ slider "pitch" 0 6.2832 0.01 d.pitch (\v -> Scene.toSource (Lorenz { d | pitch = v }))
            , colorRow "stroke" d.stroke (\s -> Scene.toSource (Lorenz { d | stroke = s }))
            ]
        , group "Presets"
            [ div [ class "mv-presets" ]
                [ preset "Classic ρ=28" (Scene.toSource (Lorenz { d | rho = 28 }))
                , preset "Stable ρ=14" (Scene.toSource (Lorenz { d | rho = 14 }))
                , preset "Wild ρ=99.96" (Scene.toSource (Lorenz { d | rho = 99.96 }))
                ]
            ]
        , div [ class "mv-hint" ] [ text "The attractor spins while it animates (yaw is the clock)." ]
        ]



-- FORCE-DIRECTED GRAPH ----------------------------------------------------------------------------


buildGraph : GraphData -> Html String
buildGraph d =
    div [ class "mv-controls" ]
        [ group "Layout"
            [ slider "iterations" 10 300 5 (toFloat d.iterations) (\v -> Scene.toSource (Graph { d | iterations = round v }))
            , colorRow "stroke" d.stroke (\s -> Scene.toSource (Graph { d | stroke = s }))
            ]
        , group "Presets"
            [ div [ class "mv-presets" ]
                [ preset "Wheel" (Scene.toSource (Graph (regraph d Scene.graphWheel)))
                , preset "Prism" (Scene.toSource (Graph (regraph d Scene.graphPrism)))
                , preset "K5" (Scene.toSource (Graph (regraph d Scene.graphComplete)))
                ]
            ]
        , div [ class "mv-hint" ] [ text "Edit nodes and edges directly in the Code tab." ]
        ]


{-| Adopt a preset graph but keep the iteration count and colour the user already chose. -}
regraph : GraphData -> GraphData -> GraphData
regraph current shape =
    { shape | iterations = current.iterations, stroke = current.stroke }



-- CELLULAR AUTOMATON ------------------------------------------------------------------------------


buildAutomaton : AutomatonData -> Html String
buildAutomaton d =
    div [ class "mv-controls" ]
        [ group "Rule"
            [ slider "rule" 0 255 1 (toFloat d.rule) (\v -> Scene.toSource (Automaton { d | rule = round v }))
            , div [ class "mv-presets" ]
                [ preset "30" (Scene.toSource (Automaton { d | rule = 30 }))
                , preset "90" (Scene.toSource (Automaton { d | rule = 90 }))
                , preset "110" (Scene.toSource (Automaton { d | rule = 110 }))
                , preset "184" (Scene.toSource (Automaton { d | rule = 184 }))
                ]
            ]
        , group "Grid"
            [ slider "width" 21 201 2 (toFloat d.width) (\v -> Scene.toSource (Automaton { d | width = round v }))
            , slider "generations" 10 160 1 (toFloat d.generations) (\v -> Scene.toSource (Automaton { d | generations = round v }))
            , colorRow "stroke" d.stroke (\s -> Scene.toSource (Automaton { d | stroke = s }))
            ]
        , div [ class "mv-hint" ] [ text "Seed cells (column offsets from centre) live in the Code tab." ]
        ]



-- SHARED CONTROLS ---------------------------------------------------------------------------------


group : String -> List (Html String) -> Html String
group title children =
    div [ class "mv-group" ] (div [ class "mv-group-title" ] [ text title ] :: children)


{-| A labelled range slider. `toSrc` turns the new value into the full new source string. -}
slider : String -> Float -> Float -> Float -> Float -> (Float -> String) -> Html String
slider label mn mx stp val toSrc =
    div [ class "mv-ctrl" ]
        [ div [ class "mv-ctrl-head" ]
            [ span [ class "mv-ctrl-label" ] [ text label ]
            , span [ class "mv-ctrl-val" ] [ text (fmt val) ]
            ]
        , input
            [ Attr.type_ "range"
            , Attr.min (String.fromFloat mn)
            , Attr.max (String.fromFloat mx)
            , Attr.step (String.fromFloat stp)
            , Attr.value (String.fromFloat val)
            , onInput (\s -> toSrc (Maybe.withDefault val (String.toFloat s)))
            ]
            []
        ]


colorRow : String -> String -> (String -> String) -> Html String
colorRow label val toSrc =
    div [ class "mv-ctrl mv-ctrl-inline" ]
        [ span [ class "mv-ctrl-label" ] [ text label ]
        , input [ Attr.type_ "color", Attr.value val, class "mv-color", onInput toSrc ] []
        ]


numCell : Float -> (Float -> String) -> Html String
numCell val toSrc =
    input
        [ Attr.type_ "number"
        , Attr.step "0.01"
        , Attr.value (String.fromFloat val)
        , class "mv-num"
        , onInput (\s -> toSrc (Maybe.withDefault val (String.toFloat s)))
        ]
        []


preset : String -> String -> Html String
preset label src =
    button [ class "mv-preset", onClick src ] [ text label ]


tableHead : List String -> Html String
tableHead cols =
    div [ class "mv-trow mv-thead" ] (List.map (\h -> span [ class "mv-th" ] [ text h ]) cols)


fmt : Float -> String
fmt x =
    String.fromFloat (toFloat (round (x * 1000)) / 1000)


updateAt : Int -> (a -> a) -> List a -> List a
updateAt i f xs =
    List.indexedMap
        (\j x ->
            if j == i then
                f x

            else
                x
        )
        xs
