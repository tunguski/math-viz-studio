module Main exposing (main)

{-| MathViz Studio — a visual builder for mathematical visualisations, built on the reusable `Editor`
shell.

You pick a visualisation in the **Gallery**, shape it with sliders in **Build**, or edit it as plain
Elm in **Code** — the three are views of one file. That file *is* the mathematical model: a single
`scene = { … }` Elm data structure, a different shape for each kind of visualisation (a harmonograph,
an iterated function system, a polyhedron). The shell supplies all the IDE chrome (editing,
resizable panes, sharing, autosave); this module just wires in the SVG result pane
(`MathVizPreview`), the builder panels (`Controls`) and the opening scene.

Baseline of what to build toward: <https://en.wikipedia.org/wiki/Mathematical_visualization>.
-}

import Controls
import Editor
import Highlight
import MathVizPreview
import Scene


main : Program () (Editor.Model MathVizPreview.Model MathVizPreview.Msg) (Editor.Msg MathVizPreview.Msg)
main =
    Editor.program
        { preview = MathVizPreview.spec
        , intel = elmIntel
        , initialFiles = [ ( "Scene.elm", Scene.harmonographStarter ) ]
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
            [ { icon = "form", title = "Build", tabs = [], view = Controls.build }
            , { icon = "wizard", title = "Gallery", tabs = [], view = Controls.gallery }
            ]
        }


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
