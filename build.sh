#!/usr/bin/env bash
#
# build.sh — build MathViz Studio.
#
# The app reuses the whole elm-editor shell (file pane, code editing, resizable panes, sharing,
# autosave) via Editor.program, plugging in its own SVG result pane and builder panels. The editor
# shell modules are declared in elm.vendored.json and resolved by the compiler at build time
# (cloned into git-deps/ at the pinned ref, or taken from the local checkout named in
# elm.vendored.local.json). Set ELM to the elm-lang CLI (default `elm`).
#
#   ELM=../../elm.sh ./build.sh
#
set -euo pipefail
cd "$(dirname "$0")"

ELM="${ELM:-elm}"
OUT="build"

# Compile the app (it type-checks cleanly, like the other elm-lang example apps). Absolute
#    paths are used because the elm.sh wrapper chdirs to the elm-lang project before running.
mkdir -p "$OUT"
P="$(pwd)"
echo "Compiling MathViz Studio with: $ELM"
$ELM make "$P/src/Main.elm" --project="$P/elm.json" -o "$P/$OUT/app.js" >/dev/null

# 2b) Compile the static Catalogue page — one section per visualisation (sample + formula + blurb),
#     generated from the same registry. Its own entry point and host page; no ports.
echo "Compiling the Catalogue page"
$ELM make "$P/src/Catalogue.elm" --project="$P/elm.json" -o "$P/$OUT/catalogue.js" >/dev/null

# 3) The editor shell's stylesheet (the .ed-* IDE chrome the host page layers its studio styles on).
#    It ships at the elm-editor repo root — the manifest gathers only .elm sources — so copy it from
#    the checkout the compiler used: the local override (elm.vendored.local.json) if present, else git-deps/.
ED="git-deps/elm-editor"
if [ -f elm.vendored.local.json ]; then
  L=$(sed -n 's/.*"elm-editor"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p' elm.vendored.local.json)
  [ -n "$L" ] && [ -d "$L" ] && ED="$L"
fi
cp "$ED/editor.css" "$OUT/editor.css"

# 4) The host pages (the studio, and the static catalogue).
cp index.template.html "$OUT/index.html"
cp catalogue.template.html "$OUT/catalogue.html"

echo "Done. Serve it with the elm-lang CLI itself (no Node needed):"
echo "  $ELM server serve.elm --static $OUT --port 8000   # then open http://localhost:8000/"
