# MathViz Studio — a visual builder for mathematical visualisations

A modern, in-browser studio for building **mathematical visualisations**, where the model being
visualised is a plain **Elm data structure** you can shape with sliders *or* edit as code — the two
stay perfectly in sync. Built on the reusable [elm-editor](../elm-editor) shell (file pane, code
editing, resizable panes, sharing, autosave); this project supplies only the visualisation domain: an
SVG result pane and the builder panels.

Baseline for what to build toward:
<https://en.wikipedia.org/wiki/Mathematical_visualization>.

## The six visualisations

Each kind of mathematics gets its **own** Elm data structure — `scene = { … }` — a different shape
per visualisation, drawn live into SVG. The kinds span the categories of the Wikipedia article above
(plane curves, fractals, geometry, chaos theory, graph theory, cellular automata):

| Kind | Mathematics | Model (`scene`) |
|---|---|---|
| **Harmonograph** | Sums of damped sinusoids — a family of looping plane curves | two lists of oscillators `{ amp, freq, phase, decay }` for x and y, `samples`, `stroke` |
| **Iterated function system** | A chaos-game fractal (the Barnsley fern, Sierpiński triangle, twin dragon) | a list of affine maps `{ a, b, c, d, e, f, p }`, `points`, `stroke` |
| **Polyhedron** | A wireframe solid, rotated and projected to the plane | `vertices : List (x, y, z)`, `edges : List (i, j)`, `yaw`, `pitch`, `stroke` |
| **Lorenz attractor** | A strange attractor — the chaotic "butterfly" flow, integrated and spun | `{ sigma, rho, beta, dt, steps, yaw, pitch, stroke }` |
| **Force-directed graph** | A network laid out by a spring (Fruchterman–Reingold) simulation | `nodes : List String`, `edges : List (i, j)`, `iterations`, `stroke` |
| **Cellular automaton** | An elementary Wolfram rule's space-time diagram (rule 30/90/110/…) | `{ rule, width, generations, seed : List Int, stroke }` |

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

## How it fits together — a registry of self-contained visualisations

The studio is configured by **one list** in `Main` — the registry. Each entry is a self-contained
module under `Viz/` that bundles a visualisation's *model, data, logic and visualisation* and exposes
a single `Viz` value. The preview, the Build controls and the Gallery all read the registry, so the
hosting modules never mention a specific visualisation.

Shared infrastructure (one place each, reused by every visualisation):

| Module | Layer | Role |
|---|---|---|
| `Value` | **data** | Reads the `scene = { … }` Elm value out of the source (`parseScene`, decode accessors) and prints values back (`header`, `renderList`, `numStr`). The model is just data — no interpreter. |
| `Draw` | **visualisation** | SVG primitives — the shared `stage`, coordinate rounding, `fitTransform`, and the 3-D `project`/`rotate2`/`centroid`. |
| `Form` | **controls** | The Build-panel widgets (`slider`, `colorRow`, `numCell`, `preset`, …); each returns `Html String` whose message *is* the new scene source. |
| `Viz` | **contract** | The plugin record `Viz` (`kind`, `name`, `description`, `starter`, `movable`, `render`, `controls`) and `find`. The model type stays hidden inside each module. |

The visualisations (each `src/Viz/*.elm` is model + decode/print + render + controls behind one `viz`):
`Viz.Harmonograph`, `Viz.Ifs`, `Viz.Polyhedron`, `Viz.Lorenz`, `Viz.Graph`, `Viz.Automaton`.

The studio shell:

| Module | Role |
|---|---|
| `MathVizPreview` | The `Preview.Spec`: reads the scene's `kind`, finds the matching `Viz` in the registry, renders it, drives the animation clock, surfaces parse errors. Registry-driven — unchanged when you add a visualisation. |
| `Controls` | The **Build** and **Gallery** panels — dispatch to the current `Viz`'s controls, and one gallery card per registry entry. Also registry-driven. |
| `Main` | Holds the `registry : List Viz` and wires the shell. |

### Add your own visualisation

1. Write `src/Viz/MyThing.elm` exposing `viz : Viz` — copy the smallest existing one
   ([`Viz/Lorenz.elm`](src/Viz/Lorenz.elm) is a good template). Define your `Model`, a `decode`
   (using `Value`), a `toSource`, a `view : … -> Svg msg` (using `Draw`), and `controls` (using
   `Form`). Give it a unique `kind` string.
2. Add `Viz.MyThing.viz` to `registry` in [`Main.elm`](src/Main.elm).

That is the whole wiring — your visualisation now has a result pane, a Build form and a Gallery card.
A user shipping their *own* set just edits that one list.

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
