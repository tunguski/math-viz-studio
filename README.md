# MathViz Studio — a visual builder for mathematical visualisations

A modern, in-browser studio for building **mathematical visualisations**, where the model being
visualised is a plain **Elm data structure** you can shape with sliders *or* edit as code — the two
stay perfectly in sync. Built on the reusable [elm-editor](../elm-editor) shell (file pane, code
editing, resizable panes, sharing, autosave); this project supplies only the visualisation domain: an
SVG result pane and the builder panels.

Baseline for what to build toward:
<https://en.wikipedia.org/wiki/Mathematical_visualization>.

## The three visualisations

Each kind of mathematics gets its **own** Elm data structure — `scene = { … }` — a different shape
per visualisation, drawn live into SVG. The three opening kinds, chosen across the categories of the
Wikipedia article above (plane curves, fractals, geometry):

| Kind | Mathematics | Model (`scene`) |
|---|---|---|
| **Harmonograph** | Sums of damped sinusoids — a family of looping plane curves | two lists of oscillators `{ amp, freq, phase, decay }` for x and y, `samples`, `stroke` |
| **Iterated function system** | A chaos-game fractal (the Barnsley fern, Sierpiński triangle, twin dragon) | a list of affine maps `{ a, b, c, d, e, f, p }`, `points`, `stroke` |
| **Polyhedron** | A wireframe solid, rotated and projected to the plane | `vertices : List (x, y, z)`, `edges : List (i, j)`, `yaw`, `pitch`, `stroke` |

For example, the harmonograph model is just data:

```elm
scene =
    { kind = "harmonograph"
    , x =
        [ { amp = 150, freq = 3, phase = 0, decay = 0.004 }
        , { amp = 90, freq = 2, phase = 1.5708, decay = 0.008 }
        ]
    , y =
        [ { amp = 150, freq = 2, phase = 0, decay = 0.004 }
        , { amp = 90, freq = 3, phase = 2.094, decay = 0.008 }
        ]
    , samples = 6000
    , stroke = "#7cdcff"
    }
```

## Three views of one model

The code pane offers three tabs, all editing the same `scene` file:

- **Build** (default) — kind-specific controls: sliders for amplitude/frequency/phase/decay, the
  affine-map table, yaw/pitch, colour pickers, and shape/fractal presets. Dragging a control
  re-prints the whole scene, so the picture moves as you drag.
- **Gallery** — cards that switch the whole visualisation (and its data structure) to another kind.
- **Code** — the raw Elm, with syntax highlighting. Edit the data structure by hand; the Build panel
  and the picture follow.

The result pane plays/pauses an animation (the curve precesses, the solid spins) and copies the model
source.

## How it fits together

| Module | Role |
|---|---|
| `Scene` | The three model data structures, a small reader of the Elm *value* sublanguage (`parse`), and a pretty-printer back to Elm source (`toSource`). The hinge every view round-trips through. |
| `Render` | `Scene → Svg` — samples the harmonograph, runs the chaos game, projects the polyhedron. |
| `MathVizPreview` | The `Preview.Spec` plugged into the shell: parse the file, render it, drive the animation clock, surface parse errors. |
| `Controls` | The **Build** and **Gallery** panels — pure views that emit a new source string the shell folds back into the file. |
| `Main` | Wires the shell: `Editor.program { preview = MathVizPreview.spec, panels = [Build, Gallery], … }`. |

There is no interpreter here — the model is *data*, not a program — so `Scene.parse` reads just
records, lists, tuples, numbers and strings, and the studio stays small.

## Build & run

You need the [elm-lang](https://github.com/tunguski/elm-lang) CLI and a checkout of
[elm-editor](../elm-editor) next door (the shell modules are vendored from it at build time).

```sh
# from this directory; ELM is the elm-lang CLI, EDITOR the elm-editor checkout
ELM=../../elm.sh EDITOR=../elm-editor ./build.sh        # or:  ./build.ps1  on Windows

# serve the built site with the elm-lang CLI itself — no Node/npx:
../../elm.sh server serve.elm --static build --port 8000   # then open http://localhost:8000/
```

`build.sh` / `build.ps1` copy the shell modules (`Highlight`, `CodeEditor`, `Share`, `Preview`,
`Editor`) into `vendor/`, compile `src/Main.elm` to `build/app.js` with `--no-check` (as the other
elm-lang example apps do), and assemble the host page. On Windows, point `ELM` at the jar, e.g.
`$env:ELM = 'java -jar ..\..\target\elm.jar'; ./build.ps1`.

The whole toolchain is the elm-lang CLI: `elm make` compiles the app, and `elm server … --static`
serves it (the `serve.elm` file is a trivial handler so the `--static build` flag serves `build/`
straight from disk). No Node, no `npx`.

## Deploying (GitHub Pages)

[`.github/workflows/pages.yml`](.github/workflows/pages.yml) builds and publishes the site on every
push to `master`/`main`. CI checks out this repo plus the sibling `tunguski/elm-lang` (the compiler)
and `tunguski/elm-editor` (the vendored shell), builds the elm-lang `elm.jar` with Maven, runs
`build.sh` (so the site is compiled by the same elm-lang CLI), and deploys `build/` to Pages. Enable
it under **Settings → Pages → Source: GitHub Actions**.

### Notes for the elm-lang JS backend

- The SVG renderer uses only the attributes the JS backend binds (`viewBox`, `points`, `d`, `stroke`,
  `strokeWidth`, `strokeLinecap`, `opacity`, `cx/cy/r`, `x1/y1/x2/y2`, …). It avoids
  `preserveAspectRatio`, `strokeLinejoin` and `strokeOpacity`, which are not bound.
- `Svg` is provided by the elm-lang runtime (like `Storage`), so it is not a package in `elm.json`.
- The compiled app auto-mounts on `<div id="app">` and exposes its one outgoing port at
  `window.$app.ports.copyToClipboard`.
