module Controls exposing (build, gallery)

{-| The two structured code-pane panels, **driven by the registry** (`List Viz`):

  - `build` finds the visualisation for the current scene's `kind` and delegates to its own controls.
  - `gallery` is a card per registered visualisation; clicking one emits its starter source, switching
    the whole scene (and its model type) to that kind.

Both emit a new source string the editor shell folds back into the file. Adding a visualisation to
the registry adds its controls and its gallery card automatically — nothing here changes.
-}

import Form
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Value
import Viz exposing (Viz)


{-| The "Build" panel: the current visualisation's own controls. -}
build : List Viz -> Int -> String -> Html String
build registry _ source =
    case Value.kindOf source of
        Ok kind ->
            case Viz.find kind registry of
                Just v ->
                    v.controls source

                Nothing ->
                    Form.note ("No visualisation is registered for kind \"" ++ kind ++ "\".")

        Err e ->
            Form.note ("This file isn't a scene yet — " ++ e)


{-| The "Gallery" panel: one card per registered visualisation. -}
gallery : List Viz -> Int -> String -> Html String
gallery registry _ _ =
    div [ class "mv-gallery" ] (List.map card registry)


card : Viz -> Html String
card v =
    button [ class "mv-card", onClick v.starter ]
        [ span [ class "mv-card-title" ] [ text v.name ]
        , span [ class "mv-card-desc" ] [ text v.description ]
        ]
