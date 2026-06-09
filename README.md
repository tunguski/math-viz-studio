# MathViz Studio — a visual builder for mathematical visualisations

A modern, in-browser studio for building **mathematical visualisations**, where the model being
visualised is a plain **Elm data structure** you can shape with sliders *or* edit as code — the two
stay perfectly in sync. Built on the reusable [elm-editor](../elm-editor) shell (file pane, code
editing, resizable panes, sharing, autosave); this project supplies only the visualisation domain: an
SVG result pane and the builder panels.

Baseline for what to build toward:
<https://en.wikipedia.org/wiki/Mathematical_visualization>.

## The visualisations

Each kind of mathematics gets its **own** Elm data structure — `scene = { … }` — a different shape
per visualisation, drawn live into SVG. The forty-two kinds span the categories of the Wikipedia
article above and beyond: plane curves, fractals, parametric surfaces, geometry and computational
geometry, chaos theory and strange attractors, complex analysis, linear algebra, topology, graph
theory, cellular automata, number theory, random walks, wave physics, space-filling curves and
formal grammars:

| Kind | Mathematics | Model (`scene`) |
|---|---|---|
| **Harmonograph** | Sums of damped sinusoids — looping plane curves | two lists of oscillators `{ amp, freq, phase, decay }`, `samples`, `stroke` |
| **Maurer rose** | Chords around a rose curve `r = sin(n·θ)`, stepped in degrees | `{ n, d, stroke }` |
| **Iterated function system** | A chaos-game fractal (Barnsley fern, Sierpiński, twin dragon) | a list of affine maps `{ a, b, c, d, e, f, p }`, `points`, `stroke` |
| **Julia set** | Escape-time fractal of `z ↦ z² + c` (complex analysis) | `{ cRe, cIm, maxIter, resolution, hue, stroke }` |
| **Polyhedron** | A wireframe solid, rotated and projected | `vertices : List (x, y, z)`, `edges : List (i, j)`, `yaw`, `pitch`, `stroke` |
| **Torus knot** | A `(p, q)` knot wound on a torus (topology), spun | `{ p, q, samples, yaw, pitch, stroke }` |
| **Lorenz attractor** | A strange attractor — the chaotic "butterfly" flow | `{ sigma, rho, beta, dt, steps, yaw, pitch, stroke }` |
| **Bifurcation diagram** | The logistic map's period-doubling route to chaos | `{ rMin, rMax, columns, iterations, settle, stroke }` |
| **Vector field** | The phase portrait of a 2×2 linear system (linear algebra) | `{ a, b, c, d, grid, stroke }` |
| **Force-directed graph** | A network laid out by a spring (Fruchterman–Reingold) simulation | `nodes : List String`, `edges : List (i, j)`, `iterations`, `stroke` |
| **Cellular automaton** | An elementary Wolfram rule's space-time diagram (rule 30/90/110/…) | `{ rule, width, generations, seed : List Int, stroke }` |
| **Mandelbrot set** | The most famous fractal — escape-time of `z ↦ z² + c`, panned and zoomed | `{ centerX, centerY, zoom, maxIter, resolution, hue, stroke }` |
| **Newton fractal** | Newton's-method root basins for `z³ − 1` (complex analysis) | `{ resolution, maxIter, hue, stroke }` |
| **Rössler attractor** | A chaotic flow — a spiralling sheet with one fold, integrated and spun | `{ a, b, c, dt, steps, yaw, pitch, stroke }` |
| **Clifford attractor** | A wing-like 2-D strange attractor (a point cloud) | `{ a, b, c, d, points, stroke }` |
| **L-system** | A rewriting grammar drawn by a turtle (Koch, dragon, plant) | `{ axiom, rules : List (String, String), angle, iterations, stroke }` |
| **Superformula** | Gielis's polar curve — circles, stars, flowers, blobs from six numbers | `{ m, n1, n2, n3, a, b, stroke }` |
| **Hilbert curve** | A space-filling curve (one line fills the plane) | `{ order, stroke }` |
| **Phyllotaxis** | A sunflower's golden-angle seed spiral (per-dot, so gradient paints it) | `{ count, angle, spread, stroke }` |
| **Ulam spiral** | Primes on a number spiral — they line up on diagonals (number theory) | `{ size, stroke }` |
| **Chladni figure** | The nodal pattern of a vibrating plate (wave physics) | `{ n, m, resolution, stroke }` |
| **Spirograph** | The looping hypotrochoid of a circle rolling inside a circle | `{ bigR, smallR, pen, samples, stroke }` |
| **Random walk** | A drunkard's wander — discrete Brownian motion | `{ steps, seed, stepSize, stroke }` |
| **Domain colouring** | A complex function `f(z)` pictured by colour (hue = its angle) | `{ fn, resolution, hue }` |
| **Matrix transform** | How a 2×2 matrix shears/rotates/scales the plane (linear algebra) | `{ a, b, c, d, lines, stroke }` |
| **Torus** | A doughnut surface as a spinning wireframe of circles | `{ bigR, smallR, segU, segV, yaw, pitch, stroke }` |
| **Spherical harmonic** | A sphere rippled by `sin(l·v)·cos(m·u)` lobes (surfaces) | `{ l, m, bump, segU, segV, yaw, pitch, stroke }` |
| **Möbius strip** | The one-sided band with a half-twist (topology) | `{ width, segU, segV, yaw, pitch, stroke }` |
| **Klein bottle** | The figure-8 immersion of the one-sided closed surface (topology) | `{ bulge, segU, segV, yaw, pitch, stroke }` |
| **Surface plot** | The graph `z = f(x, y)` — saddle, monkey saddle, ripple, peak | `{ fn, warp, segU, segV, yaw, pitch, stroke }` |
| **Lissajous figure** | Two perpendicular oscillations `x = sin(a·t+δ), y = sin(b·t)` | `{ freqX, freqY, phase, samples, stroke }` |
| **Epicycloid** | A point on a circle rolling around a circle (cardioid, nephroid) | `{ bigR, smallR, samples, stroke }` |
| **De Jong attractor** | An airy trigonometric strange-attractor cloud | `{ a, b, c, d, points, stroke }` |
| **Hénon map** | A thin banded strange attractor with fractal cross-section | `{ a, b, points, stroke }` |
| **Singular values** | A matrix maps the unit circle to an ellipse — its SVD axes (linear algebra) | `{ a, b, c, d, stroke }` |
| **Eigenvectors** | The invariant directions a matrix only stretches (linear algebra) | `{ a, b, c, d, stroke }` |
| **Polynomial intersections** | Plot several polynomials and list where they cross (solving the systems pᵢ = pⱼ) | `polys : List { coeffs : List Float, color }`, `xMin`, `xMax`, `yMin`, `yMax` |
| **Lévy flight** | A heavy-tailed random walk — local clusters and long jumps | `{ steps, seed, alpha, stroke }` |
| **Voronoi diagram** | The plane carved into cells by nearest site (geometry) | `sites : List (x, y)`, `resolution`, `hue` |
| **Truchet tiles** | One arc tile in two random orientations, flowing into loops | `{ size, seed, stroke }` |
| **Ford circles** | A tangent circle packing of the fractions (number theory) | `{ maxQ, stroke }` |
| **Convex hull** | The rubber-band polygon around a random scatter (computational geometry) | `{ count, seed, stroke }` |

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

## Colouring

The base `stroke` colour is part of the model; *how* it evolves is a view choice, picked with the
🎨 button in the result pane (the dynamic modes use the animation clock, so press ▶ Animate too).
The modes are a small pluggable registry in [`Color`](src/Color.elm) — add a branch to `resolve`
(and a name to `modes`) and every visualisation gets it:

- **Fixed** — the base colour, unchanged (the original behaviour).
- **Cycle** — the whole figure's hue rotates over time (animates even a static figure, like the fern).
- **Gradient** — the hue sweeps along the figure's natural parameter (most striking on the curves).
- **Pulse** — a bright band travels along the figure, driven by the clock.

A `Coloring` is `Uniform` (one colour for the figure, maybe time-varying) or `Varying` (a colour per
position), so a renderer draws a single cheap stroke when the colour doesn't vary along the figure and
only bands the path ([`Draw.curve`](src/Draw.elm)) when it does. The Julia set, already escape-time
coloured, hue-rotates its whole palette under the time modes.

## How it fits together — a registry of self-contained visualisations

The studio is configured by **one list** in `Main` — the registry. Each entry is a self-contained
module under `Viz/` that bundles a visualisation's *model, data, logic and visualisation* and exposes
a single `Viz` value. The preview, the Build controls and the Gallery all read the registry, so the
hosting modules never mention a specific visualisation.

Shared infrastructure (one place each, reused by every visualisation):

| Module | Layer | Role |
|---|---|---|
| `Value` | **data** | Reads the `scene = { … }` Elm value out of the source (`parseScene`, decode accessors) and prints values back (`header`, `renderList`, `numStr`). The model is just data — no interpreter. |
| `Draw` | **visualisation** | SVG primitives — the shared `stage`, coordinate rounding, `fitTransform`, the 3-D `project`/`rotate2`/`centroid`, and `curve` (a colourable, optionally banded polyline). |
| `Color` | **visualisation** | The pluggable colour-evolution registry: `fixed`/`cycle`/`gradient`/`pulse`, resolved from a base colour to a `Coloring`. |
| `Form` | **controls** | The Build-panel widgets (`slider`, `colorRow`, `numCell`, `preset`, …); each returns `Html String` whose message *is* the new scene source. |
| `Viz` | **contract** | The plugin record `Viz` (`kind`, `name`, `description`, `starter`, `movable`, `render`, `controls`) and `find`. The model type stays hidden inside each module. |

The visualisations (each `src/Viz/*.elm` is model + decode/print + render + controls behind one `viz`):
`Harmonograph`, `MaurerRose`, `Superformula`, `Ifs`, `Julia`, `Mandelbrot`, `Newton`, `LSystem`,
`Hilbert`, `Polyhedron`, `TorusKnot`, `Lorenz`, `Rossler`, `Clifford`, `Bifurcation`, `VectorField`,
`Phyllotaxis`, `Ulam`, `Chladni`, `Graph`, `Automaton`.

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

The build also compiles `src/Catalogue.elm` to `build/catalogue.js` and emits `catalogue.html`: a
**static reference page** generated from the same registry, with one section per visualisation — a
live sample, the defining formula (drawn as SVG), and the description. Open it at `/catalogue.html`
(there's a link from the studio, bottom-right).

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
