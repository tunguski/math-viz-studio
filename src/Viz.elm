module Viz exposing (Viz, find)

{-| The **plugin contract**: everything the studio needs to host one visualisation, with the model
type hidden inside the module that provides it. The studio is configured with a `List Viz` (the
registry), and adding a visualisation is just writing a module that exposes one of these and listing
it in `Main`.

Every operation speaks in terms of the **scene source string** (the editable Elm data structure), so
the registry is uniform even though each visualisation has a different model type:

  - `kind` — the discriminator matching the scene's `kind = "…"` field; the registry dispatches on it.
  - `name` / `description` — shown on the gallery card.
  - `starter` — the source the gallery card switches to.
  - `movable` — whether the visualisation uses the animation clock (so the preview only ticks for
    the kinds that actually move).
  - `render` — parse the source **once** and return a drawing function `Float -> Svg`: given the
    animation phase, produce the SVG. Splitting it this way lets a visualisation do its
    phase-independent work (parsing, and any heavy precompute like integrating the Lorenz flow) once,
    so an animation frame only runs the cheap per-phase transform — no re-parsing or re-integrating
    60 times a second. A static visualisation just ignores the phase. The SVG message type is `Never`;
    the preview maps it into its own messages.
  - `controls` — the Build panel: a form that re-prints the scene source on every change.

@docs Viz, find

-}

import Html exposing (Html)
import Svg exposing (Svg)


{-| One hostable visualisation. -}
type alias Viz =
    { kind : String
    , name : String
    , description : String
    , starter : String
    , movable : Bool
    , render : String -> Result String (Float -> Svg Never)
    , controls : String -> Html String
    }


{-| Find the visualisation in a registry that handles the given `kind`. -}
find : String -> List Viz -> Maybe Viz
find kind registry =
    case registry of
        v :: rest ->
            if v.kind == kind then
                Just v

            else
                find kind rest

        [] ->
            Nothing
