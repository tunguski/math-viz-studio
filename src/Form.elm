module Form exposing
    ( controls, group, note, subhead, hint
    , slider, colorRow, numCell, preset, presets, table, row, tableHead
    , updateAt
    )

{-| Shared **control widgets** for the Build panels. Every widget produces `Html String`: its message
*is* the new full scene source, which the editor shell folds back into the file. A visualisation's
`controls` only has to wire its sliders to functions that re-print its model — never any state.

@docs controls, group
@docs slider, colorRow, numCell, preset, tableHead
@docs updateAt

-}

import Html exposing (Html, button, div, input, span, text)
import Html.Attributes as Attr exposing (class)
import Html.Events exposing (onClick, onInput)


{-| The outer container for a visualisation's controls. -}
controls : List (Html String) -> Html String
controls =
    div [ class "mv-controls" ]


{-| A red notice, e.g. when the source no longer decodes into this visualisation's model. -}
note : String -> Html String
note message =
    div [ class "mv-controls" ] [ div [ class "mv-error" ] [ text message ] ]


{-| A titled group of controls. -}
group : String -> List (Html String) -> Html String
group title children =
    div [ class "mv-group" ] (div [ class "mv-group-title" ] [ text title ] :: children)


{-| A small sub-heading inside a group (e.g. "oscillator 1"). -}
subhead : String -> Html String
subhead label =
    div [ class "mv-subhead" ] [ text label ]


{-| A dim hint line, e.g. "edit this in the Code tab". -}
hint : String -> Html String
hint message =
    div [ class "mv-hint" ] [ text message ]


{-| A labelled range slider. `toSrc` turns the new value into the full new scene source. -}
slider : String -> Float -> Float -> Float -> Float -> (Float -> String) -> Html String
slider label mn mx stp val toSrc =
    div [ class "mv-ctrl" ]
        [ div [ class "mv-ctrl-head" ]
            [ span [ class "mv-ctrl-label" ] [ text label ]
            , span [ class "mv-ctrl-val" ] [ text (fmt val) ]
            ]
        , input
            [ Attr.type_ "range"
            , Attr.min (String.fromFloat mn)
            , Attr.max (String.fromFloat mx)
            , Attr.step (String.fromFloat stp)
            , Attr.value (String.fromFloat val)
            , onInput (\s -> toSrc (Maybe.withDefault val (String.toFloat s)))
            ]
            []
        ]


{-| A labelled colour picker. -}
colorRow : String -> String -> (String -> String) -> Html String
colorRow label val toSrc =
    div [ class "mv-ctrl mv-ctrl-inline" ]
        [ span [ class "mv-ctrl-label" ] [ text label ]
        , input [ Attr.type_ "color", Attr.value val, class "mv-color", onInput toSrc ] []
        ]


{-| A compact numeric cell (for tables of values, like the affine maps). -}
numCell : Float -> (Float -> String) -> Html String
numCell val toSrc =
    input
        [ Attr.type_ "number"
        , Attr.step "0.01"
        , Attr.value (String.fromFloat val)
        , class "mv-num"
        , onInput (\s -> toSrc (Maybe.withDefault val (String.toFloat s)))
        ]
        []


{-| A button that swaps the whole scene to `src` (a preset). -}
preset : String -> String -> Html String
preset label src =
    button [ class "mv-preset", onClick src ] [ text label ]


{-| A wrapping row of preset buttons. -}
presets : List (Html String) -> Html String
presets =
    div [ class "mv-presets" ]


{-| A grid of value cells (e.g. the affine-map table). -}
table : List (Html String) -> Html String
table =
    div [ class "mv-table" ]


{-| One row of value cells. -}
row : List (Html String) -> Html String
row =
    div [ class "mv-trow" ]


{-| A table header row of column labels. -}
tableHead : List String -> Html String
tableHead cols =
    div [ class "mv-trow mv-thead" ] (List.map (\h -> span [ class "mv-th" ] [ text h ]) cols)


fmt : Float -> String
fmt x =
    String.fromFloat (toFloat (round (x * 1000)) / 1000)


{-| Replace the element at index `i` by applying `f` to it. -}
updateAt : Int -> (a -> a) -> List a -> List a
updateAt i f xs =
    List.indexedMap
        (\j x ->
            if j == i then
                f x

            else
                x
        )
        xs
