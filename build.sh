#!/usr/bin/env bash
#
# build.sh — build MathViz Studio.
#
# The app reuses the whole elm-editor shell (file pane, code editing, resizable panes, sharing,
# autosave) via Editor.program, plugging in its own SVG result pane and builder panels. Since Elm
# has no cross-project imports, we copy the shell modules we need into vendor/ (a source-directory
# listed in elm.json) before compiling. Set EDITOR to the elm-editor checkout (default ../elm-editor)
# and ELM to the elm-lang CLI (default `elm`).
#
#   ELM=../../elm.sh ./build.sh
#
set -euo pipefail
cd "$(dirname "$0")"

ELM="${ELM:-elm}"
EDITOR="${EDITOR:-../elm-editor}"
OUT="build"

# 1) Vendor the editor shell modules (and the Elm highlighter) from elm-editor.
mkdir -p vendor
for m in Highlight CodeEditor Share Preview Editor; do
  if [ ! -f "$EDITOR/src/$m.elm" ]; then
    echo "build.sh: missing $EDITOR/src/$m.elm — set EDITOR to the elm-editor checkout" >&2
    exit 1
  fi
  cp "$EDITOR/src/$m.elm" "vendor/$m.elm"
done

# 2) Compile the app (the editor widget leans on idioms the strict type checker doesn't fully
#    analyse, so — like the other elm-lang example apps — we compile with --no-check). Absolute paths
#    are used because the elm.sh wrapper chdirs to the elm-lang project before running.
mkdir -p "$OUT"
P="$(pwd)"
echo "Compiling MathViz Studio with: $ELM"
$ELM make "$P/src/Main.elm" --project="$P/elm.json" -o "$P/$OUT/app.js" --no-check >/dev/null

# 2b) Compile the static Catalogue page — one section per visualisation (sample + formula + blurb),
#     generated from the same registry. Its own entry point and host page; no ports.
echo "Compiling the Catalogue page"
$ELM make "$P/src/Catalogue.elm" --project="$P/elm.json" -o "$P/$OUT/catalogue.js" --no-check >/dev/null

# 3) The editor shell's stylesheet (the .ed-* IDE chrome the host page layers its studio styles on).
cp "$EDITOR/editor.css" "$OUT/editor.css"

# 4) The host pages (the studio, and the static catalogue).
cp index.template.html "$OUT/index.html"
cp catalogue.template.html "$OUT/catalogue.html"

echo "Done. Serve it with the elm-lang CLI itself (no Node needed):"
echo "  $ELM server serve.elm --static $OUT --port 8000   # then open http://localhost:8000/"
