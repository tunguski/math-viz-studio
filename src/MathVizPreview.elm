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
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, classList, title)
import Html.Events exposing (onClick)
import Preview exposing (Context)
import Value
import Viz exposing (Viz)


{-| Copy the given text (the model source) to the clipboard. -}
port copyToClipboard : String -> Cmd msg


{-| The pane's own state: the visualisation and drawing function for the last source that parsed (so
animation frames never re-parse), any current error, and the animation clock. -}
type alias Model =
    { current : Maybe Viz
    , draw : Maybe (Float -> Html Never)
    , error : Maybe String
    , phase : Float
    , animate : Bool
    }


type Msg
    = Tick Float
    | ToggleAnimate
    | Copy String


{-| The pluggable preview, parameterised by the visualisation registry. -}
spec : List Viz -> Preview.Spec Model Msg
spec registry =
    { init = \ctx -> ( reparse registry ctx { current = Nothing, draw = Nothing, error = Nothing, phase = 0, animate = False }, Cmd.none )
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

        Copy source ->
            ( model, copyToClipboard source )


{-| Tick only when running and the current visualisation declares itself movable. -}
subscriptions : Context -> Model -> Sub Msg
subscriptions _ model =
    case model.current of
        Just v ->
            if model.animate && v.movable then
                Browser.Events.onAnimationFrameDelta Tick

            else
                Sub.none

        Nothing ->
            Sub.none


view : Context -> Model -> Html Msg
view ctx model =
    div [ class "mv-pane" ]
        [ toolbar ctx model
        , case model.error of
            Just e ->
                div [ class "mv-error" ] [ text ("Cannot render — " ++ e ++ "  (fix it in the Code tab)") ]

            Nothing ->
                text ""
        , div [ class "mv-stage" ] [ picture model ]
        ]


{-| Draw the cached drawer at the current phase — the whole per-frame cost during animation. -}
picture : Model -> Html Msg
picture model =
    case model.draw of
        Just drawer ->
            Html.map never (drawer model.phase)

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
            [ classList [ ( "mv-btn", True ), ( "active", model.animate ) ]
            , onClick ToggleAnimate
            , title "Play or pause the animation (the curve precesses, the solid spins)"
            ]
            [ text
                (if model.animate then
                    "⏸ Pause"

                 else
                    "▶ Animate"
                )
            ]
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
