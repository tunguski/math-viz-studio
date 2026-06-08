module Value exposing
    ( V
    , parseScene, kindOf
    , record, field, num, int, str, bool, list, tuple
    , numField, intField, strField, listField, traverse
    , header, numStr, renderList
    )

{-| The shared **data layer**: reading the `scene = { … }` Elm value out of a source string, and
printing values back to Elm source. Every visualisation module decodes its own model through these
helpers and prints its own source with them, so the parser/printer live in exactly one place and a
new visualisation never has to write its own.

This is a small, self-contained reader of the Elm *value* sublanguage (records, lists, tuples,
numbers, strings, bools) — not a full interpreter; a model is just data.

@docs V
@docs parseScene, kindOf
@docs record, field, num, int, str, bool, list, tuple
@docs numField, intField, strField, listField, traverse
@docs header, numStr, renderList

-}


{-| A parsed Elm value. Opaque: decode it with the accessors below rather than pattern-matching. -}
type V
    = VNum Float
    | VStr String
    | VBool Bool
    | VList (List V)
    | VTup (List V)
    | VRec (List ( String, V ))



-- PARSING -----------------------------------------------------------------------------------------


{-| Read the `scene = …` definition's value from an Elm source string. -}
parseScene : String -> Result String V
parseScene src =
    case sceneBody src of
        Nothing ->
            Err "No `scene = …` definition found."

        Just body ->
            case pValue (String.toList body) of
                Ok ( v, _ ) ->
                    Ok v

                Err e ->
                    Err e


{-| The value of the scene's `kind` field — used to pick the right visualisation from the registry
without fully decoding the model. -}
kindOf : String -> Result String String
kindOf src =
    parseScene src
        |> Result.andThen record
        |> Result.andThen (strField "kind")


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
            takeWhile (\c -> Char.isDigit c || c == '.') cs1
    in
    case String.toFloat (sign ++ digits) of
        Just n ->
            Ok ( VNum n, cs2 )

        Nothing ->
            Err ("Bad number: " ++ sign ++ digits)


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



-- DECODING ACCESSORS ------------------------------------------------------------------------------


{-| A record's fields, in order. -}
record : V -> Result String (List ( String, V ))
record v =
    case v of
        VRec fs ->
            Ok fs

        _ ->
            Err "Expected a record"


{-| A number. -}
num : V -> Result String Float
num v =
    case v of
        VNum n ->
            Ok n

        _ ->
            Err "Expected a number"


{-| A number, rounded to an integer. -}
int : V -> Result String Int
int v =
    Result.map round (num v)


{-| A string literal. -}
str : V -> Result String String
str v =
    case v of
        VStr s ->
            Ok s

        _ ->
            Err "Expected a string"


{-| A boolean. -}
bool : V -> Result String Bool
bool v =
    case v of
        VBool b ->
            Ok b

        _ ->
            Err "Expected True or False"


{-| A list's elements. -}
list : V -> Result String (List V)
list v =
    case v of
        VList xs ->
            Ok xs

        _ ->
            Err "Expected a list"


{-| A tuple's elements (e.g. a `( x, y, z )` vertex or `( i, j )` edge). -}
tuple : V -> Result String (List V)
tuple v =
    case v of
        VTup xs ->
            Ok xs

        _ ->
            Err "Expected a tuple"


{-| Look up a field by name in a record's fields. -}
field : String -> List ( String, V ) -> Result String V
field name fs =
    case fs of
        ( n, v ) :: rest ->
            if n == name then
                Ok v

            else
                field name rest

        [] ->
            Err ("Missing field: " ++ name)


numField : String -> List ( String, V ) -> Result String Float
numField name fs =
    field name fs |> Result.andThen num


intField : String -> List ( String, V ) -> Result String Int
intField name fs =
    field name fs |> Result.andThen int


strField : String -> List ( String, V ) -> Result String String
strField name fs =
    field name fs |> Result.andThen str


listField : String -> List ( String, V ) -> Result String (List V)
listField name fs =
    field name fs |> Result.andThen list


{-| Decode every element of a list, failing on the first error. -}
traverse : (a -> Result e b) -> List a -> Result e (List b)
traverse f xs =
    List.foldr (\x acc -> Result.map2 (::) (f x) acc) (Ok []) xs



-- PRINTING ----------------------------------------------------------------------------------------


{-| The module header every printed scene starts with, up to and including `scene =`. -}
header : String
header =
    "module Scene exposing (scene)\n\n\nscene =\n"


{-| A number, as Elm would write it: integral values lose their trailing `.0` (`String.fromFloat`
already does this), so `160` stays `160` and `0.16` stays `0.16`. -}
numStr : Float -> String
numStr =
    String.fromFloat


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
