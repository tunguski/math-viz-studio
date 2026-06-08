port module MathVizPreview exposing (Model, Msg, spec)

{-| The result pane for MathViz Studio, plugged into the reusable `Editor` shell as a `Preview.Spec`.

It parses the currently selected file into a `Scene` (the mathematical model) and renders it as SVG.
A small toolbar plays/pauses an animation clock — the harmonograph precesses and the polyhedron spins
— and copies the model source. When the source does not parse, the pane keeps showing the last good
picture and surfaces the error as a banner. The shell owns all the editing chrome; this module owns
only the result column.
-}

import Browser.Events
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, classList, title)
import Html.Events exposing (onClick)
import Preview exposing (Context)
import Render
import Scene exposing (Scene)


{-| Copy the given text (the model source) to the clipboard. -}
port copyToClipboard : String -> Cmd msg


{-| The pane's own state: the last successfully parsed scene, the animation phase and whether it is
running, and any current parse error. -}
type alias Model =
    { scene : Maybe Scene
    , error : Maybe String
    , phase : Float
    , animate : Bool
    }


type Msg
    = Tick Float
    | ToggleAnimate
    | Copy String


{-| The pluggable preview MathViz Studio wires into `Editor.program`. -}
spec : Preview.Spec Model Msg
spec =
    { init = \ctx -> ( reparse ctx { scene = Nothing, error = Nothing, phase = 0, animate = False }, Cmd.none )
    , sourcesChanged = \ctx model -> ( reparse ctx model, Cmd.none )
    , update = update
    , subscriptions = subscriptions
    , view = view
    , error = .error
    , onAddFile = Nothing
    , takeNewFile = \_ -> Nothing
    }


{-| Re-read the selected source into a scene, keeping the previous picture if it no longer parses. -}
reparse : Context -> Model -> Model
reparse ctx model =
    case Scene.parse (currentSource ctx) of
        Ok scene ->
            { model | scene = Just scene, error = Nothing }

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


{-| Animate only when running and only for kinds that move — never re-run the (heavy) chaos game on a
clock. -}
subscriptions : Context -> Model -> Sub Msg
subscriptions _ model =
    if model.animate && movable model.scene then
        Browser.Events.onAnimationFrameDelta Tick

    else
        Sub.none


movable : Maybe Scene -> Bool
movable scene =
    case scene of
        Just (Scene.Harmonograph _) ->
            True

        Just (Scene.Polyhedron _) ->
            True

        Just (Scene.Lorenz _) ->
            True

        _ ->
            -- IFS, graph and automaton are static (no phase term in their render)
            False


view : Context -> Model -> Html Msg
view ctx model =
    div [ class "mv-pane" ]
        [ toolbar ctx model
        , case model.error of
            Just e ->
                div [ class "mv-error" ] [ text ("Cannot render — " ++ e ++ "  (fix it in the Code tab)") ]

            Nothing ->
                text ""
        , div [ class "mv-stage" ]
            [ case model.scene of
                Just scene ->
                    Render.view model.phase scene

                Nothing ->
                    div [ class "mv-empty" ] [ text "No scene to show." ]
            ]
        ]


toolbar : Context -> Model -> Html Msg
toolbar ctx model =
    div [ class "mv-toolbar" ]
        [ case model.scene of
            Just scene ->
                span [ class "mv-tag" ] [ text (Scene.kindName scene) ]

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
