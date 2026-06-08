module Static exposing (handle)

{-| A do-nothing HTTP handler so the elm-lang CLI can serve the built site as **static files** — no
Node/npx needed. Run:

    elm server serve.elm --static build --port 8000

The `--static build` flag serves `build/` (index.html, app.js, editor.css) before this handler is
consulted, so every real request is answered from disk; this handler only catches anything left over.
-}

import Server exposing (Request, Response, notFound)


handle : Request -> Response
handle _ =
    notFound
