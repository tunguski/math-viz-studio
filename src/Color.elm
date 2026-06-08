module Color exposing (Coloring(..), resolve, solid, modes, label, timeVarying)

{-| **Pluggable colour-evolution functions.** A visualisation has a base colour (its `stroke`); a
`Coloring` decides how that base is turned into the actual colour(s) drawn, possibly varying along the
figure and/or over the animation clock.

The set of modes is a small registry — add a branch to `resolve` (and a name to `modes`/`label`) to
plug in a new way of colouring, and every visualisation picks it up.

  - **fixed** — the base colour, unchanged (what the studio did before).
  - **cycle** — the whole figure's hue rotates over time.
  - **gradient** — the hue sweeps along the figure (its natural parameter `t ∈ [0, 1]`).
  - **pulse** — a bright band travels along the figure, driven by the clock.

A `Coloring` is either `Uniform` (one colour for the whole figure, maybe time-varying) or `Varying`
(a colour per position `t`), so a renderer can cheaply draw a single stroke when the colour does not
vary along the figure, and only band the figure when it does.

@docs Coloring, resolve, solid, modes, label, timeVarying

-}


{-| How a base colour becomes drawn colour(s). -}
type Coloring
    = Uniform (Float -> String) -- phase -> colour (the same across the whole figure)
    | Varying (Float -> Float -> String) -- t, phase -> colour (varies along the figure)


{-| The registry of mode names, in menu order. -}
modes : List String
modes =
    [ "fixed", "cycle", "gradient", "pulse" ]


{-| A human label for a mode. -}
label : String -> String
label mode =
    case mode of
        "fixed" ->
            "Fixed"

        "cycle" ->
            "Cycle"

        "gradient" ->
            "Gradient"

        "pulse" ->
            "Pulse"

        _ ->
            mode


{-| Does this mode change over the animation clock? (So the preview knows to keep ticking even when
the geometry itself is static.) -}
timeVarying : String -> Bool
timeVarying mode =
    mode == "cycle" || mode == "pulse"


{-| Build the coloring for a mode over a base colour (a `#rrggbb` hex). Unknown modes fall back to
fixed. -}
resolve : String -> String -> Coloring
resolve mode base =
    let
        ( h, s, l ) =
            toHsl base
    in
    case mode of
        "cycle" ->
            Uniform (\phase -> hsl (h + phase * 60) s l)

        "gradient" ->
            Varying (\t _ -> hsl (h + t * 320) s l)

        "pulse" ->
            Varying
                (\t phase ->
                    let
                        pos =
                            frac (phase * 0.25)

                        raw =
                            abs (t - pos)

                        dist =
                            min raw (1 - raw)

                        boost =
                            max 0 (1 - dist / 0.16) * 0.4
                    in
                    hsl h s (l + boost)
                )

        _ ->
            Uniform (\_ -> base)


{-| A single representative colour from a coloring (for figures drawn as one element). -}
solid : Coloring -> Float -> String
solid coloring phase =
    case coloring of
        Uniform f ->
            f phase

        Varying f ->
            f 0 phase



-- HSL HELPERS -------------------------------------------------------------------------------------


hsl : Float -> Float -> Float -> String
hsl h s l =
    "hsl(" ++ String.fromInt (modBy 360 (round h)) ++ ", " ++ pct s ++ ", " ++ pct l ++ ")"


pct : Float -> String
pct x =
    String.fromInt (round (clampF 0 1 x * 100)) ++ "%"


clampF : Float -> Float -> Float -> Float
clampF lo hi x =
    max lo (min hi x)


frac : Float -> Float
frac x =
    x - toFloat (floor x)


{-| Convert a `#rrggbb` hex to HSL (h in degrees, s and l in [0, 1]). Non-hex input falls back to a
neutral grey, so an exotic base never crashes the colour ramp. -}
toHsl : String -> ( Float, Float, Float )
toHsl input =
    let
        hex =
            if String.startsWith "#" input then
                String.dropLeft 1 input

            else
                input
    in
    if String.length hex < 6 then
        ( 0, 0, 0.6 )

    else
        let
            r =
                toFloat (hexByte (String.slice 0 2 hex)) / 255

            g =
                toFloat (hexByte (String.slice 2 4 hex)) / 255

            b =
                toFloat (hexByte (String.slice 4 6 hex)) / 255

            mx =
                max r (max g b)

            mn =
                min r (min g b)

            l =
                (mx + mn) / 2

            d =
                mx - mn

            s =
                if d == 0 then
                    0

                else
                    d / (1 - abs (2 * l - 1))

            h =
                if d == 0 then
                    0

                else if mx == r then
                    60 * mod6 ((g - b) / d)

                else if mx == g then
                    60 * ((b - r) / d + 2)

                else
                    60 * ((r - g) / d + 4)
        in
        ( h, s, l )


mod6 : Float -> Float
mod6 x =
    x - 6 * toFloat (floor (x / 6))


hexByte : String -> Int
hexByte s =
    case String.toList s of
        [ c1, c0 ] ->
            16 * hexDigit c1 + hexDigit c0

        _ ->
            0


hexDigit : Char -> Int
hexDigit c =
    let
        code =
            Char.toCode (Char.toLower c)
    in
    if code >= 48 && code <= 57 then
        code - 48

    else if code >= 97 && code <= 102 then
        code - 97 + 10

    else
        0
