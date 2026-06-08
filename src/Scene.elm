module Scene exposing
    ( Scene(..)
    , HarmonographData, Pendulum
    , IfsData, Affine
    , PolyhedronData, Vec3
    , parse, toSource, kindName
    , harmonographStarter, ifsStarter, polyhedronStarter
    , ifsFern, ifsSierpinski, ifsDragon
    , cube, tetrahedron, octahedron
    )

{-| The **mathematical model** that each visualisation renders — and the bridge between it and the
Elm source the user edits.

A scene is one of three independent data structures, each a faithful Elm record literal: a
**harmonograph** (sums of damped sinusoids — a family of plane curves), an **iterated function
system** (a chaos-game fractal such as the Barnsley fern), and a **polyhedron** wireframe (vertices
and edges of a solid, projected). The visual builder is bidirectional, so this module is the hinge:

  - `parse` reads the `scene = { … }` definition out of an Elm source string into a typed `Scene`.
    It is a small, self-contained reader of the Elm *value* sublanguage (records, lists, tuples,
    numbers, strings) — not a full interpreter; the model is just data.
  - `toSource` prints a `Scene` back to a complete, readable Elm module, so the slider/colour
    controls can regenerate the source the editor and the renderer both read.

Because the source round-trips through this one typed representation, the code pane, the form
controls and the live SVG always agree on a single model.
-}


{-| A whole visualisation, tagged by its kind. Each variant carries a different Elm data structure,
because each kind of mathematics wants a different model. -}
type Scene
    = Harmonograph HarmonographData
    | Ifs IfsData
    | Polyhedron PolyhedronData


{-| A harmonograph: two lists of damped oscillators driving the x and y of a pen. The traced curve is
`x(t) = Σ amp·sin(freq·t + phase)·e^(−decay·t)` (and likewise for y), sampled `samples` times. -}
type alias HarmonographData =
    { x : List Pendulum
    , y : List Pendulum
    , samples : Int
    , stroke : String
    }


{-| One damped sinusoid — a single pendulum of the harmonograph. -}
type alias Pendulum =
    { amp : Float, freq : Float, phase : Float, decay : Float }


{-| An iterated function system: a set of affine maps, each chosen with probability `p` in a chaos
game that scatters `points` dots. The classic example is the Barnsley fern. -}
type alias IfsData =
    { maps : List Affine
    , points : Int
    , stroke : String
    }


{-| One affine map `(x, y) ↦ (a·x + b·y + e, c·x + d·y + f)`, applied with probability `p`. -}
type alias Affine =
    { a : Float, b : Float, c : Float, d : Float, e : Float, f : Float, p : Float }


{-| A polyhedron as a wireframe: vertices in 3-space, the edges joining them by index, and the yaw /
pitch the renderer rotates it by before projecting to the plane. -}
type alias PolyhedronData =
    { vertices : List Vec3
    , edges : List ( Int, Int )
    , yaw : Float
    , pitch : Float
    , stroke : String
    }


{-| A point in 3-space. -}
type alias Vec3 =
    { x : Float, y : Float, z : Float }


{-| A human label for a scene's kind, for the preview toolbar. -}
kindName : Scene -> String
kindName scene =
    case scene of
        Harmonograph _ ->
            "harmonograph"

        Ifs _ ->
            "iterated function system"

        Polyhedron _ ->
            "polyhedron"



-- PARSING (Elm source → Scene) --------------------------------------------------------------------


{-| Read the `scene = …` definition from an Elm source string into a typed `Scene`, or explain why
it could not. -}
parse : String -> Result String Scene
parse src =
    case sceneBody src of
        Nothing ->
            Err "No `scene = …` definition found."

        Just body ->
            case pValue (String.toList body) of
                Ok ( v, _ ) ->
                    decodeScene v

                Err e ->
                    Err e


{-| The text of the `scene` definition's right-hand side: everything after the first `=` that follows
the first lowercase `scene` token (the one in `exposing (scene)` or the definition itself). -}
sceneBody : String -> Maybe String
sceneBody src =
    case String.indexes "scene" src of
        i :: _ ->
            let
                after =
                    String.dropLeft i src
            in
            case String.indexes "=" after of
                j :: _ ->
                    Just (String.dropLeft (j + 1) after)

                [] ->
                    Nothing

        [] ->
            Nothing


{-| The Elm value sublanguage we read: just enough to express a scene record. -}
type V
    = VNum Float
    | VStr String
    | VBool Bool
    | VList (List V)
    | VTup (List V)
    | VRec (List ( String, V ))


{-| Parse one value off the front of the input, returning it and the unconsumed remainder. -}
pValue : List Char -> Result String ( V, List Char )
pValue cs0 =
    let
        cs =
            dropWs cs0
    in
    case cs of
        [] ->
            Err "Unexpected end of input"

        c :: rest ->
            if c == '{' then
                pRecord (dropWs rest) []

            else if c == '[' then
                pList (dropWs rest)

            else if c == '(' then
                pTuple rest []

            else if c == '"' then
                pString rest []

            else if c == '-' || c == '.' || Char.isDigit c then
                pNumber cs

            else if c == 'T' || c == 'F' then
                pBool cs

            else
                Err ("Unexpected character: " ++ String.fromChar c)


{-| Drop whitespace and `--` line comments. -}
dropWs : List Char -> List Char
dropWs cs =
    case cs of
        c :: rest ->
            if c == ' ' || c == '\n' || c == '\t' || c == '\u{000D}' then
                dropWs rest

            else if c == '-' then
                case rest of
                    '-' :: more ->
                        dropWs (dropLine more)

                    _ ->
                        cs

            else
                cs

        [] ->
            cs


dropLine : List Char -> List Char
dropLine cs =
    case cs of
        '\n' :: rest ->
            rest

        _ :: rest ->
            dropLine rest

        [] ->
            []


pRecord : List Char -> List ( String, V ) -> Result String ( V, List Char )
pRecord cs acc =
    case cs of
        '}' :: rest ->
            Ok ( VRec (List.reverse acc), rest )

        _ ->
            let
                ( name, cs2 ) =
                    takeIdent cs
            in
            if name == "" then
                Err "Expected a record field name"

            else
                case dropWs cs2 of
                    '=' :: cs3 ->
                        case pValue cs3 of
                            Ok ( v, cs4 ) ->
                                case dropWs cs4 of
                                    ',' :: cs5 ->
                                        pRecord (dropWs cs5) (( name, v ) :: acc)

                                    '}' :: cs5 ->
                                        Ok ( VRec (List.reverse (( name, v ) :: acc)), cs5 )

                                    _ ->
                                        Err "Expected , or } in record"

                            Err e ->
                                Err e

                    _ ->
                        Err ("Expected = after field " ++ name)


pList : List Char -> Result String ( V, List Char )
pList cs =
    case cs of
        ']' :: rest ->
            Ok ( VList [], rest )

        _ ->
            pListItems cs []


pListItems : List Char -> List V -> Result String ( V, List Char )
pListItems cs acc =
    case pValue cs of
        Ok ( v, cs2 ) ->
            case dropWs cs2 of
                ',' :: cs3 ->
                    pListItems (dropWs cs3) (v :: acc)

                ']' :: cs3 ->
                    Ok ( VList (List.reverse (v :: acc)), cs3 )

                _ ->
                    Err "Expected , or ] in list"

        Err e ->
            Err e


pTuple : List Char -> List V -> Result String ( V, List Char )
pTuple cs acc =
    case pValue cs of
        Ok ( v, cs2 ) ->
            case dropWs cs2 of
                ',' :: cs3 ->
                    pTuple cs3 (v :: acc)

                ')' :: cs3 ->
                    case List.reverse (v :: acc) of
                        [ single ] ->
                            -- a parenthesised value, not a tuple
                            Ok ( single, cs3 )

                        items ->
                            Ok ( VTup items, cs3 )

                _ ->
                    Err "Expected , or ) in tuple"

        Err e ->
            Err e


pString : List Char -> List Char -> Result String ( V, List Char )
pString cs acc =
    case cs of
        '\\' :: c :: more ->
            pString more (c :: acc)

        '"' :: more ->
            Ok ( VStr (String.fromList (List.reverse acc)), more )

        c :: more ->
            pString more (c :: acc)

        [] ->
            Err "Unterminated string"


pNumber : List Char -> Result String ( V, List Char )
pNumber cs =
    let
        ( sign, cs1 ) =
            case cs of
                '-' :: rest ->
                    ( "-", rest )

                _ ->
                    ( "", cs )

        ( digits, cs2 ) =
            takeWhile isNumChar cs1

        str =
            sign ++ digits
    in
    case String.toFloat str of
        Just n ->
            Ok ( VNum n, cs2 )

        Nothing ->
            Err ("Bad number: " ++ str)


isNumChar : Char -> Bool
isNumChar c =
    Char.isDigit c || c == '.'


pBool : List Char -> Result String ( V, List Char )
pBool cs =
    if startsWith "True" cs then
        Ok ( VBool True, List.drop 4 cs )

    else if startsWith "False" cs then
        Ok ( VBool False, List.drop 5 cs )

    else
        Err "Expected True or False"


startsWith : String -> List Char -> Bool
startsWith s cs =
    List.take (String.length s) cs == String.toList s


takeIdent : List Char -> ( String, List Char )
takeIdent =
    takeWhile (\c -> Char.isAlphaNum c || c == '_')


takeWhile : (Char -> Bool) -> List Char -> ( String, List Char )
takeWhile pred cs0 =
    let
        go acc cs =
            case cs of
                c :: more ->
                    if pred c then
                        go (c :: acc) more

                    else
                        ( String.fromList (List.reverse acc), cs )

                [] ->
                    ( String.fromList (List.reverse acc), [] )
    in
    go [] cs0



-- DECODING (V → Scene) ----------------------------------------------------------------------------


decodeScene : V -> Result String Scene
decodeScene v =
    asRec v
        |> Result.andThen
            (\fs ->
                strField "kind" fs
                    |> Result.andThen
                        (\k ->
                            case k of
                                "harmonograph" ->
                                    Result.map Harmonograph (decodeHarmonograph fs)

                                "ifs" ->
                                    Result.map Ifs (decodeIfs fs)

                                "polyhedron" ->
                                    Result.map Polyhedron (decodePoly fs)

                                _ ->
                                    Err ("Unknown kind: " ++ k)
                        )
            )


decodeHarmonograph : List ( String, V ) -> Result String HarmonographData
decodeHarmonograph fs =
    Result.map4 HarmonographData
        (listField "x" fs |> Result.andThen (traverse decodePendulum))
        (listField "y" fs |> Result.andThen (traverse decodePendulum))
        (intField "samples" fs)
        (strField "stroke" fs)


decodePendulum : V -> Result String Pendulum
decodePendulum v =
    asRec v
        |> Result.andThen
            (\fs ->
                Result.map4 Pendulum
                    (numField "amp" fs)
                    (numField "freq" fs)
                    (numField "phase" fs)
                    (numField "decay" fs)
            )


decodeIfs : List ( String, V ) -> Result String IfsData
decodeIfs fs =
    Result.map3 IfsData
        (listField "maps" fs |> Result.andThen (traverse decodeAffine))
        (intField "points" fs)
        (strField "stroke" fs)


decodeAffine : V -> Result String Affine
decodeAffine v =
    asRec v
        |> Result.andThen
            (\fs ->
                Result.map5 (\a b c d e -> ( a, b, ( c, d, e ) ))
                    (numField "a" fs)
                    (numField "b" fs)
                    (numField "c" fs)
                    (numField "d" fs)
                    (numField "e" fs)
                    |> Result.andThen
                        (\( a, b, ( c, d, e ) ) ->
                            Result.map2 (\f p -> Affine a b c d e f p)
                                (numField "f" fs)
                                (numField "p" fs)
                        )
            )


decodePoly : List ( String, V ) -> Result String PolyhedronData
decodePoly fs =
    Result.map5 PolyhedronData
        (listField "vertices" fs |> Result.andThen (traverse decodeVec3))
        (listField "edges" fs |> Result.andThen (traverse decodeEdge))
        (numField "yaw" fs)
        (numField "pitch" fs)
        (strField "stroke" fs)


decodeVec3 : V -> Result String Vec3
decodeVec3 v =
    case v of
        VTup [ a, b, c ] ->
            Result.map3 Vec3 (asNum a) (asNum b) (asNum c)

        _ ->
            Err "Expected a 3-tuple vertex like ( 1, 0, -1 )"


decodeEdge : V -> Result String ( Int, Int )
decodeEdge v =
    case v of
        VTup [ a, b ] ->
            Result.map2 Tuple.pair
                (Result.map round (asNum a))
                (Result.map round (asNum b))

        _ ->
            Err "Expected a 2-tuple edge like ( 0, 1 )"



-- decode helpers


asRec : V -> Result String (List ( String, V ))
asRec v =
    case v of
        VRec fs ->
            Ok fs

        _ ->
            Err "Expected a record"


asNum : V -> Result String Float
asNum v =
    case v of
        VNum n ->
            Ok n

        _ ->
            Err "Expected a number"


asStr : V -> Result String String
asStr v =
    case v of
        VStr s ->
            Ok s

        _ ->
            Err "Expected a string"


asList : V -> Result String (List V)
asList v =
    case v of
        VList xs ->
            Ok xs

        _ ->
            Err "Expected a list"


getField : String -> List ( String, V ) -> Result String V
getField name fs =
    case fs of
        ( n, v ) :: rest ->
            if n == name then
                Ok v

            else
                getField name rest

        [] ->
            Err ("Missing field: " ++ name)


numField : String -> List ( String, V ) -> Result String Float
numField name fs =
    getField name fs |> Result.andThen asNum


intField : String -> List ( String, V ) -> Result String Int
intField name fs =
    numField name fs |> Result.map round


strField : String -> List ( String, V ) -> Result String String
strField name fs =
    getField name fs |> Result.andThen asStr


listField : String -> List ( String, V ) -> Result String (List V)
listField name fs =
    getField name fs |> Result.andThen asList


traverse : (a -> Result e b) -> List a -> Result e (List b)
traverse f xs =
    List.foldr (\x acc -> Result.map2 (::) (f x) acc) (Ok []) xs



-- PRINTING (Scene → Elm source) -------------------------------------------------------------------


{-| Print a scene as a complete, readable Elm module — the inverse of `parse`, used by the form
controls to write their changes back into the editor. -}
toSource : Scene -> String
toSource scene =
    case scene of
        Harmonograph d ->
            harmonographSource d

        Ifs d ->
            ifsSource d

        Polyhedron d ->
            polySource d


header : String
header =
    "module Scene exposing (scene)\n\n\nscene =\n"


{-| A number, as Elm would write it: integral values lose their trailing `.0` (`String.fromFloat`
already does this), so `160` stays `160` and `0.16` stays `0.16`. -}
n : Float -> String
n =
    String.fromFloat


harmonographSource : HarmonographData -> String
harmonographSource d =
    header
        ++ "    { kind = \"harmonograph\"\n"
        ++ "    , x =\n        "
        ++ renderList "        " (List.map pendulumStr d.x)
        ++ "\n    , y =\n        "
        ++ renderList "        " (List.map pendulumStr d.y)
        ++ "\n    , samples = "
        ++ String.fromInt d.samples
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"


pendulumStr : Pendulum -> String
pendulumStr p =
    "{ amp = " ++ n p.amp ++ ", freq = " ++ n p.freq ++ ", phase = " ++ n p.phase ++ ", decay = " ++ n p.decay ++ " }"


ifsSource : IfsData -> String
ifsSource d =
    header
        ++ "    { kind = \"ifs\"\n"
        ++ "    , maps =\n        "
        ++ renderList "        " (List.map affineStr d.maps)
        ++ "\n    , points = "
        ++ String.fromInt d.points
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"


affineStr : Affine -> String
affineStr m =
    "{ a = " ++ n m.a ++ ", b = " ++ n m.b ++ ", c = " ++ n m.c ++ ", d = " ++ n m.d ++ ", e = " ++ n m.e ++ ", f = " ++ n m.f ++ ", p = " ++ n m.p ++ " }"


polySource : PolyhedronData -> String
polySource d =
    header
        ++ "    { kind = \"polyhedron\"\n"
        ++ "    , vertices =\n        "
        ++ renderList "        " (List.map vecStr d.vertices)
        ++ "\n    , edges =\n        "
        ++ renderList "        " (List.map edgeStr d.edges)
        ++ "\n    , yaw = "
        ++ n d.yaw
        ++ "\n    , pitch = "
        ++ n d.pitch
        ++ "\n    , stroke = \""
        ++ d.stroke
        ++ "\"\n    }\n"


vecStr : Vec3 -> String
vecStr v =
    "( " ++ n v.x ++ ", " ++ n v.y ++ ", " ++ n v.z ++ " )"


edgeStr : ( Int, Int ) -> String
edgeStr ( i, j ) =
    "( " ++ String.fromInt i ++ ", " ++ String.fromInt j ++ " )"


{-| Lay a list of already-rendered items out one per line, Elm-style: `[ first`, then `, item`
lines, then a closing `]`, each continuation indented by `indent`. -}
renderList : String -> List String -> String
renderList indent items =
    case items of
        [] ->
            "[]"

        first :: rest ->
            "[ "
                ++ first
                ++ String.concat (List.map (\it -> "\n" ++ indent ++ ", " ++ it) rest)
                ++ "\n"
                ++ indent
                ++ "]"



-- STARTERS & PRESETS ------------------------------------------------------------------------------


{-| The three gallery starters, as source the editor opens with / switches to. -}
harmonographStarter : String
harmonographStarter =
    toSource
        (Harmonograph
            { x =
                [ { amp = 150, freq = 3, phase = 0, decay = 0.004 }
                , { amp = 90, freq = 2, phase = 1.5708, decay = 0.008 }
                ]
            , y =
                [ { amp = 150, freq = 2, phase = 0, decay = 0.004 }
                , { amp = 90, freq = 3, phase = 2.094, decay = 0.008 }
                ]
            , samples = 6000
            , stroke = "#7cdcff"
            }
        )


ifsStarter : String
ifsStarter =
    toSource (Ifs ifsFern)


polyhedronStarter : String
polyhedronStarter =
    toSource (Polyhedron cube)


{-| The Barnsley fern — the textbook iterated function system. -}
ifsFern : IfsData
ifsFern =
    { maps =
        [ { a = 0, b = 0, c = 0, d = 0.16, e = 0, f = 0, p = 0.01 }
        , { a = 0.85, b = 0.04, c = -0.04, d = 0.85, e = 0, f = 1.6, p = 0.85 }
        , { a = 0.2, b = -0.26, c = 0.23, d = 0.22, e = 0, f = 1.6, p = 0.07 }
        , { a = -0.15, b = 0.28, c = 0.26, d = 0.24, e = 0, f = 0.44, p = 0.07 }
        ]
    , points = 14000
    , stroke = "#7cfc9b"
    }


{-| The Sierpiński triangle as three half-scale corner maps. -}
ifsSierpinski : IfsData
ifsSierpinski =
    { maps =
        [ { a = 0.5, b = 0, c = 0, d = 0.5, e = 0, f = 0, p = 0.34 }
        , { a = 0.5, b = 0, c = 0, d = 0.5, e = 1, f = 0, p = 0.33 }
        , { a = 0.5, b = 0, c = 0, d = 0.5, e = 0.5, f = 0.87, p = 0.33 }
        ]
    , points = 14000
    , stroke = "#ffd479"
    }


{-| The twin dragon curve — two rotating, half-scale maps. -}
ifsDragon : IfsData
ifsDragon =
    { maps =
        [ { a = 0.5, b = -0.5, c = 0.5, d = 0.5, e = 0, f = 0, p = 0.5 }
        , { a = -0.5, b = -0.5, c = 0.5, d = -0.5, e = 1, f = 0, p = 0.5 }
        ]
    , points = 16000
    , stroke = "#ff9cee"
    }


{-| The cube — eight corners of [-1, 1]³ and the twelve edges between them. -}
cube : PolyhedronData
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


{-| The regular tetrahedron, on alternate cube corners. -}
tetrahedron : PolyhedronData
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


{-| The regular octahedron — the six unit points on the axes. -}
octahedron : PolyhedronData
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
