module Main exposing (main)

{-| MathViz Studio — a visual builder for mathematical visualisations, built on the reusable `Editor`
shell and a **registry** of visualisations.

The whole studio is configured by one list — `registry` below. Each entry is a self-contained
visualisation module (under `Viz/`) that bundles its model, its decode/print, its SVG render and its
controls behind a single `Viz` value. To add your own visualisation:

 1. write `src/Viz/MyThing.elm` exposing `viz : Viz` (copy any existing one as a template), and
 2. add `Viz.MyThing.viz` to `registry`.

That is the only wiring — the preview, the Build controls and the Gallery all read the registry, so a
new visualisation gets its result pane, its form and its gallery card for free. The file the user
edits *is* the mathematical model: a single `scene = { … }` Elm data structure, a different shape per
visualisation.

Baseline of what to build toward: <https://en.wikipedia.org/wiki/Mathematical_visualization>.
-}

import Controls
import Editor
import Highlight
import MathVizPreview
import Viz exposing (Viz)
import Viz.Automaton
import Viz.Bifurcation
import Viz.Graph
import Viz.Harmonograph
import Viz.Ifs
import Viz.Julia
import Viz.Lorenz
import Viz.MaurerRose
import Viz.Polyhedron
import Viz.TorusKnot
import Viz.VectorField


{-| The configured set of visualisations. Append your own `Viz` here. -}
registry : List Viz
registry =
    [ Viz.Harmonograph.viz
    , Viz.MaurerRose.viz
    , Viz.Ifs.viz
    , Viz.Julia.viz
    , Viz.Polyhedron.viz
    , Viz.TorusKnot.viz
    , Viz.Lorenz.viz
    , Viz.Bifurcation.viz
    , Viz.VectorField.viz
    , Viz.Graph.viz
    , Viz.Automaton.viz
    ]


main : Program () (Editor.Model MathVizPreview.Model MathVizPreview.Msg) (Editor.Msg MathVizPreview.Msg)
main =
    Editor.program
        { preview = MathVizPreview.spec registry
        , intel = elmIntel
        , initialFiles = [ ( "Scene.elm", opening ) ]
        , urls = []
        , libUrls = []
        , title = "MathViz Studio"
        , tagline = "build a visualisation — the model is an Elm data structure"
        , sessionKey = "math-viz-studio"
        , fileBrowser = False
        , backLink = Nothing
        , panels =
            -- Title-bar order; the shell appends the plain "Code" editor last. "Build" is the default
            -- view (panels[0]), so the studio opens on its sliders.
            [ { icon = "form", title = "Build", tabs = [], view = Controls.build registry }
            , { icon = "wizard", title = "Gallery", tabs = [], view = Controls.gallery registry }
            ]
        }


{-| The scene the editor opens with — the first registered visualisation's starter. -}
opening : String
opening =
    case registry of
        v :: _ ->
            v.starter

        [] ->
            "scene = {}"


{-| The scene is written in Elm, so reuse Elm syntax highlighting in the code pane. There is no
interpreter here, so autocomplete and error-location are no-ops (parse errors surface in the preview
pane instead). -}
elmIntel : Editor.CodeIntel
elmIntel =
    { highlight = Highlight.segments
    , completions = \_ _ -> []
    , accept = \source caret _ -> ( source, caret )
    , locate = \_ _ -> Nothing
    }
