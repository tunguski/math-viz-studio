port module MathVizPreview exposing (Model, Msg, spec)

{-| The result pane, plugged into the editor shell as a `Preview.Spec`, now **driven by the
registry** (`List Viz`) rather than a fixed set of kinds. It reads the scene's `kind`, finds the
matching visualisation, and renders it — so hosting a new visualisation needs no change here, only a
new entry in the registry that `Main` passes in.

A small toolbar plays/pauses the animation clock (the kinds that declare `movable = True` precess or
spin) and copies the model source. When the source does not parse, the pane keeps the last good
picture and surfaces the error as a banner.
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


{-| The pane's own state: the last source that rendered, any current error, and the animation clock. -}
type alias Model =
    { lastGood : Maybe String
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
    { init = \ctx -> ( reparse registry ctx { lastGood = Nothing, error = Nothing, phase = 0, animate = False }, Cmd.none )
    , sourcesChanged = \ctx model -> ( reparse registry ctx model, Cmd.none )
    , update = update
    , subscriptions = subscriptions registry
    , view = view registry
    , error = .error
    , onAddFile = Nothing
    , takeNewFile = \_ -> Nothing
    }


{-| Validate the current source against the registry; keep the last good picture on failure. -}
reparse : List Viz -> Context -> Model -> Model
reparse registry ctx model =
    let
        source =
            currentSource ctx
    in
    case renderSource registry 0 source of
        Ok _ ->
            { model | lastGood = Just source, error = Nothing }

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
subscriptions : List Viz -> Context -> Model -> Sub Msg
subscriptions registry _ model =
    case currentViz registry model of
        Just v ->
            if model.animate && v.movable then
                Browser.Events.onAnimationFrameDelta Tick

            else
                Sub.none

        Nothing ->
            Sub.none


view : List Viz -> Context -> Model -> Html Msg
view registry ctx model =
    div [ class "mv-pane" ]
        [ toolbar registry ctx model
        , case model.error of
            Just e ->
                div [ class "mv-error" ] [ text ("Cannot render — " ++ e ++ "  (fix it in the Code tab)") ]

            Nothing ->
                text ""
        , div [ class "mv-stage" ] [ picture registry model ]
        ]


toolbar : List Viz -> Context -> Model -> Html Msg
toolbar registry ctx model =
    div [ class "mv-toolbar" ]
        [ case currentViz registry model of
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


picture : List Viz -> Model -> Html Msg
picture registry model =
    case model.lastGood of
        Just source ->
            case renderSource registry model.phase source of
                Ok svg ->
                    Html.map never svg

                Err _ ->
                    div [ class "mv-empty" ] [ text "No scene to show." ]

        Nothing ->
            div [ class "mv-empty" ] [ text "No scene to show." ]


{-| Render a source against the registry: find the visualisation for its `kind`, then draw it. -}
renderSource : List Viz -> Float -> String -> Result String (Html Never)
renderSource registry phase source =
    Value.kindOf source
        |> Result.andThen
            (\k ->
                case Viz.find k registry of
                    Just v ->
                        v.render phase source

                    Nothing ->
                        Err
                            ("Unknown kind: \""
                                ++ k
                                ++ "\". Known kinds: "
                                ++ String.join ", " (List.map .kind registry)
                            )
            )


{-| The visualisation handling the current (last good) scene, if any. -}
currentViz : List Viz -> Model -> Maybe Viz
currentViz registry model =
    model.lastGood
        |> Maybe.andThen (\s -> Result.toMaybe (Value.kindOf s))
        |> Maybe.andThen (\k -> Viz.find k registry)


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
