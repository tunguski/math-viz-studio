module Viz.Polyhedron exposing (viz)

{-| **Polyhedron** — a wireframe solid: vertices in 3-space, the edges joining them by index, and the
yaw/pitch the renderer rotates it by before projecting (the yaw also spins with the animation clock).
-}

import Array exposing (Array)
import Draw exposing (Vec3)
import Form
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as A
import Value exposing (V)
import Viz exposing (Viz)


type alias Model =
    { vertices : List Vec3
    , edges : List ( Int, Int )
    , yaw : Float
    , pitch : Float
    , stroke : String
    }


viz : Viz
viz =
    { kind = "polyhedron"
    , name = "Polyhedron"
    , description = "A rotating wireframe solid you can spin."
    , starter = toSource cube
    , movable = True
    , render = \source -> Result.map (\m -> \phase -> view phase m) (decode source)
    , controls = controls
    }



-- DECODE / PRINT ----------------------------------------------------------------------------------


decode : String -> Result String Model
decode source =
    Value.parseScene source
        |> Result.andThen Value.record
        |> Result.andThen
            (\fs ->
                Result.map5 Model
                    (Value.listField "vertices" fs |> Result.andThen (Value.traverse vertex))
                    (Value.listField "edges" fs |> Result.andThen (Value.traverse edge))
                    (Value.numField "yaw" fs)
                    (Value.numField "pitch" fs)
                    (Value.strField "stroke" fs)
            )


vertex : V -> Result String Vec3
vertex v =
    Value.tuple v
        |> Result.andThen
            (\xs ->
                case xs of
                    [ a, b, c ] ->
                        Result.map3 (\x y z -> { x = x, y = y, z = z }) (Value.num a) (Value.num b) (Value.num c)

                    _ ->
                        Err "Expected a 3-tuple vertex like ( 1, 0, -1 )"
            )


edge : V -> Result String ( Int, Int )
edge v =
    Value.tuple v
        |> Result.andThen
            (\xs ->
                case xs of
                    [ a, b ] ->
                        Result.map2 Tuple.pair (Value.int a) (Value.int b)

                    _ ->
                        Err "Expected a 2-tuple edge like ( 0, 1 )"
            )


toSource : Model -> String
toSource d =
    Value.header
        ++ "    { kind = \"polyhedron\"\n"
        ++ "    , vertices =\n        "
        ++ Value.renderList "        " (List.map vecStr d.vertices)
        ++ "\n    , edges =\n        "
        ++ Value.renderList "        " (List.map edgeStr d.edges)
        ++ "\n    , yaw = "
        ++ Value.numStr d.yaw
        ++ "\n    , pitch = "
        ++ Value.numStr d.pitch
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"


vecStr : Vec3 -> String
vecStr v =
    "( " ++ Value.numStr v.x ++ ", " ++ Value.numStr v.y ++ ", " ++ Value.numStr v.z ++ " )"


edgeStr : ( Int, Int ) -> String
edgeStr ( i, j ) =
    "( " ++ String.fromInt i ++ ", " ++ String.fromInt j ++ " )"



-- RENDER ------------------------------------------------------------------------------------------


view : Float -> Model -> Svg msg
view phase d =
    let
        projected =
            Array.fromList (List.map (Draw.project (d.yaw + phase) d.pitch) d.vertices)

        line ( i, j ) =
            case ( Array.get i projected, Array.get j projected ) of
                ( Just ( x1, y1 ), Just ( x2, y2 ) ) ->
                    Just
                        (Svg.line
                            [ A.x1 (Draw.r2 x1)
                            , A.y1 (Draw.r2 y1)
                            , A.x2 (Draw.r2 x2)
                            , A.y2 (Draw.r2 y2)
                            , A.stroke d.stroke
                            , A.strokeWidth "2"
                            , A.opacity "0.9"
                            , A.strokeLinecap "round"
                            ]
                            []
                        )

                _ ->
                    Nothing

        dot ( x, y ) =
            Svg.circle [ A.cx (Draw.r2 x), A.cy (Draw.r2 y), A.r "3.2", A.fill d.stroke ] []
    in
    Draw.stage (List.filterMap line d.edges ++ List.map dot (Array.toList projected))



-- CONTROLS ----------------------------------------------------------------------------------------


controls : String -> Html String
controls source =
    case decode source of
        Err e ->
            Form.note ("This file isn't a polyhedron yet — " ++ e)

        Ok d ->
            Form.controls
                [ Form.group "Orientation"
                    [ Form.slider "yaw" 0 6.2832 0.01 d.yaw (\v -> toSource { d | yaw = v })
                    , Form.slider "pitch" 0 6.2832 0.01 d.pitch (\v -> toSource { d | pitch = v })
                    , Form.colorRow "stroke" d.stroke (\s -> toSource { d | stroke = s })
                    ]
                , Form.group "Shape"
                    [ Form.presets
                        [ Form.preset "Cube" (toSource (reshape d cube))
                        , Form.preset "Tetrahedron" (toSource (reshape d tetrahedron))
                        , Form.preset "Octahedron" (toSource (reshape d octahedron))
                        ]
                    ]
                , Form.hint "Edit vertices and edges directly in the Code tab."
                ]


{-| Adopt a preset's geometry but keep the orientation and colour the user already chose. -}
reshape : Model -> Model -> Model
reshape current shape =
    { shape | yaw = current.yaw, pitch = current.pitch, stroke = current.stroke }



-- PRESETS -----------------------------------------------------------------------------------------


cube : Model
cube =
    { vertices =
        [ { x = -1, y = -1, z = -1 }
        , { x = 1, y = -1, z = -1 }
        , { x = 1, y = 1, z = -1 }
        , { x = -1, y = 1, z = -1 }
        , { x = -1, y = -1, z = 1 }
        , { x = 1, y = -1, z = 1 }
        , { x = 1, y = 1, z = 1 }
        , { x = -1, y = 1, z = 1 }
        ]
    , edges =
        [ ( 0, 1 ), ( 1, 2 ), ( 2, 3 ), ( 3, 0 )
        , ( 4, 5 ), ( 5, 6 ), ( 6, 7 ), ( 7, 4 )
        , ( 0, 4 ), ( 1, 5 ), ( 2, 6 ), ( 3, 7 )
        ]
    , yaw = 0.6
    , pitch = 0.5
    , stroke = "#ff9cee"
    }


tetrahedron : Model
tetrahedron =
    { vertices =
        [ { x = 1, y = 1, z = 1 }
        , { x = 1, y = -1, z = -1 }
        , { x = -1, y = 1, z = -1 }
        , { x = -1, y = -1, z = 1 }
        ]
    , edges =
        [ ( 0, 1 ), ( 0, 2 ), ( 0, 3 ), ( 1, 2 ), ( 1, 3 ), ( 2, 3 ) ]
    , yaw = 0.6
    , pitch = 0.4
    , stroke = "#7cdcff"
    }


octahedron : Model
octahedron =
    { vertices =
        [ { x = 1, y = 0, z = 0 }
        , { x = -1, y = 0, z = 0 }
        , { x = 0, y = 1, z = 0 }
        , { x = 0, y = -1, z = 0 }
        , { x = 0, y = 0, z = 1 }
        , { x = 0, y = 0, z = -1 }
        ]
    , edges =
        [ ( 0, 2 ), ( 0, 3 ), ( 0, 4 ), ( 0, 5 )
        , ( 1, 2 ), ( 1, 3 ), ( 1, 4 ), ( 1, 5 )
        , ( 2, 4 ), ( 4, 3 ), ( 3, 5 ), ( 5, 2 )
        ]
    , yaw = 0.7
    , pitch = 0.5
    , stroke = "#7cfc9b"
    }
