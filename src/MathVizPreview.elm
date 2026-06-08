port module MathVizPreview exposing (Model, Msg, spec)

{-| The result pane, plugged into the editor shell as a `Preview.Spec`, **driven by the registry**
(`List Viz`).

For smooth animation it parses lazily: when the source changes it resolves the scene's `kind`, finds
the matching visualisation and asks it for a **drawing function** (`Float -> Svg`) — done once, so any
heavy phase-independent work (parsing, integrating the Lorenz flow, …) happens a single time. That
function and the matching `Viz` are cached on the model, so an animation frame only bumps `phase` and
calls the cached drawer: no parsing, no re-integrating per frame.

A small toolbar plays/pauses the animation clock (the kinds that declare `movable = True` precess or
spin) and copies the model source. When the source does not parse, the pane keeps the last good
drawer and surfaces the error as a banner.
-}

import Browser.Events
import Color
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, classList, title)
import Html.Events exposing (onClick)
import Preview exposing (Context)
import Value
import Viz exposing (Drawer, Viz)


{-| Copy the given text (the model source) to the clipboard. -}
port copyToClipboard : String -> Cmd msg


{-| The pane's own state: the visualisation and drawing function for the last source that parsed (so
animation frames never re-parse), any current error, the colour mode (a `Color` mode name) and the
animation clock. -}
type alias Model =
    { current : Maybe Viz
    , draw : Maybe Drawer
    , error : Maybe String
    , colorMode : String
    , phase : Float
    , animate : Bool
    , showInfo : Bool
    }


type Msg
    = Tick Float
    | ToggleAnimate
    | NextColor
    | ToggleInfo
    | Copy String


{-| The pluggable preview, parameterised by the visualisation registry. -}
spec : List Viz -> Preview.Spec Model Msg
spec registry =
    { init = \ctx -> ( reparse registry ctx { current = Nothing, draw = Nothing, error = Nothing, colorMode = "fixed", phase = 0, animate = False, showInfo = False }, Cmd.none )
    , sourcesChanged = \ctx model -> ( reparse registry ctx model, Cmd.none )
    , update = update
    , subscriptions = subscriptions
    , view = view
    , error = .error
    , onAddFile = Nothing
    , takeNewFile = \_ -> Nothing
    }


{-| Resolve the current source to a `(Viz, drawer)` once; keep the last good drawer on failure. This
is the only place that parses — animation never reaches it. -}
reparse : List Viz -> Context -> Model -> Model
reparse registry ctx model =
    let
        source =
            currentSource ctx
    in
    case Value.kindOf source of
        Err e ->
            { model | error = Just e }

        Ok kind ->
            case Viz.find kind registry of
                Nothing ->
                    { model
                        | error =
                            Just
                                ("No visualisation is registered for kind \""
                                    ++ kind
                                    ++ "\". Known: "
                                    ++ String.join ", " (List.map .kind registry)
                                )
                    }

                Just v ->
                    case v.render source of
                        Ok drawer ->
                            { model | current = Just v, draw = Just drawer, error = Nothing }

                        Err e ->
                            { model | error = Just e }


update : Context -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        Tick dt ->
            ( { model | phase = model.phase + dt * 0.0006 }, Cmd.none )

        ToggleAnimate ->
            ( { model | animate = not model.animate }, Cmd.none )

        NextColor ->
            ( { model | colorMode = nextMode model.colorMode }, Cmd.none )

        ToggleInfo ->
            ( { model | showInfo = not model.showInfo }, Cmd.none )

        Copy source ->
            ( model, copyToClipboard source )


{-| The next colour mode in the registry, wrapping around. -}
nextMode : String -> String
nextMode mode =
    let
        step ms =
            case ms of
                a :: b :: rest ->
                    if a == mode then
                        b

                    else
                        step (b :: rest)

                _ ->
                    List.head Color.modes |> Maybe.withDefault "fixed"
    in
    step Color.modes


{-| Tick when running and either the geometry moves (`movable`) or the colour mode evolves over time
(cycle / pulse) — so a static figure can still animate its colour. -}
subscriptions : Context -> Model -> Sub Msg
subscriptions _ model =
    case model.current of
        Just v ->
            if model.animate && (v.movable || Color.timeVarying model.colorMode) then
                Browser.Events.onAnimationFrameDelta Tick

            else
                Sub.none

        Nothing ->
            Sub.none


view : Context -> Model -> Html Msg
view ctx model =
    div [ class "mv-pane" ]
        ([ toolbar ctx model
         , case model.error of
            Just e ->
                div [ class "mv-error" ] [ text ("Cannot render — " ++ e ++ "  (fix it in the Code tab)") ]

            Nothing ->
                text ""
         , div [ class "mv-stage" ] [ picture model ]
         ]
            ++ (case ( model.showInfo, model.current ) of
                    ( True, Just v ) ->
                        [ infoOverlay v ]

                    _ ->
                        []
               )
        )


{-| A full-window, closeable overlay explaining the current structure, with a static sample rendered
from its starter (fixed colour, no animation). Positioned `fixed` (see editor.css overlay) so it
covers the whole editor, not just the result pane. -}
infoOverlay : Viz -> Html Msg
infoOverlay v =
    div [ class "mv-info" ]
        [ div [ class "mv-info-card" ]
            [ button [ class "mv-info-close", onClick ToggleInfo, title "Close" ] [ text "✕" ]
            , div [ class "mv-info-title" ] [ text v.name ]
            , div [ class "mv-info-sample" ] [ infoSample v ]
            , div [ class "mv-info-body" ]
                (List.map (\para -> div [ class "mv-info-p" ] [ text para ])
                    (List.filter (\p -> p /= "") (String.split "\n\n" v.about))
                )
            ]
        ]


{-| The canonical sample picture: the visualisation's starter, fixed colour, phase 0. -}
infoSample : Viz -> Html Msg
infoSample v =
    case v.render v.starter of
        Ok drawer ->
            Html.map never (drawer "fixed" 0)

        Err _ ->
            text ""


{-| Draw the cached drawer at the current colour mode and phase — the whole per-frame cost. -}
picture : Model -> Html Msg
picture model =
    case model.draw of
        Just drawer ->
            Html.map never (drawer model.colorMode model.phase)

        Nothing ->
            div [ class "mv-empty" ] [ text "No scene to show." ]


toolbar : Context -> Model -> Html Msg
toolbar ctx model =
    div [ class "mv-toolbar" ]
        [ case model.current of
            Just v ->
                span [ class "mv-tag" ] [ text v.name ]

            Nothing ->
                text ""
        , button
            [ classList [ ( "mv-btn", True ), ( "active", model.colorMode /= "fixed" ) ]
            , onClick NextColor
            , title "Colour: cycle through fixed → cycle → gradient → pulse (the dynamic modes use the clock)"
            ]
            [ text ("🎨 " ++ Color.label model.colorMode) ]
        , button
            [ classList [ ( "mv-btn", True ), ( "active", model.animate ) ]
            , onClick ToggleAnimate
            , title "Play or pause the clock (the geometry moves; cycle/pulse colours evolve)"
            ]
            [ text
                (if model.animate then
                    "⏸ Pause"

                 else
                    "▶ Animate"
                )
            ]
        , button
            [ class "mv-btn", onClick ToggleInfo, title "About this structure — its history and importance" ]
            [ text "? Info" ]
        , button
            [ class "mv-btn", onClick (Copy (currentSource ctx)), title "Copy the model's Elm source" ]
            [ text "⧉ Copy model" ]
        ]


currentSource : Context -> String
currentSource ctx =
    lookup ctx.selected ctx.files |> Maybe.withDefault ""


lookup : String -> List ( String, String ) -> Maybe String
lookup name files =
    case files of
        ( n, content ) :: rest ->
            if n == name then
                Just content

            else
                lookup name rest

        [] ->
            Nothing
