module Render exposing (view)

{-| Turn a `Scene` (the mathematical model) into SVG.

Each kind is drawn into one centred, square viewBox (`-300 -300 600 600`, so the origin is the
middle and the unit is a pixel). A `phase` argument — advanced by the preview's animation clock —
lets the curve precess and the polyhedron spin; static kinds simply ignore it.

  - **Harmonograph**: sample `x(t), y(t)` and stroke the path as one `<polyline>`.
  - **Iterated function system**: run the chaos game to a cloud of points, auto-fit its bounding box
    to the frame, and draw the whole cloud as a single `<path>` of tiny dots (one DOM node, so even
    tens of thousands of points stay cheap to render).
  - **Polyhedron**: rotate every vertex by yaw/pitch, project with a little perspective, and stroke
    the edges as `<line>`s with the vertices as dots.
  - **Lorenz attractor**: integrate the flow, rotate the 3-D trajectory by yaw/pitch and stroke it as
    one `<polyline>` (the yaw spins with `phase`).
  - **Force-directed graph**: relax a spring layout, then draw edges as `<line>`s and nodes as
    labelled dots.
  - **Cellular automaton**: evolve the Wolfram rule and draw every live cell as one filled `<path>`.

-}

import Array exposing (Array)
import Scene
    exposing
        ( Affine
        , AutomatonData
        , GraphData
        , HarmonographData
        , IfsData
        , LorenzData
        , PolyhedronData
        , Scene(..)
        , Vec3
        )
import Svg exposing (Svg)
import Svg.Attributes as A


{-| Render a scene at the given animation phase (radians). -}
view : Float -> Scene -> Svg msg
view phase scene =
    case scene of
        Harmonograph d ->
            harmonograph phase d

        Ifs d ->
            ifs d

        Polyhedron d ->
            polyhedron phase d

        Lorenz d ->
            lorenz phase d

        Graph d ->
            graph d

        Automaton d ->
            automaton d


{-| The shared frame: a centred 600×600 viewBox the renderers draw into. -}
stage : List (Svg msg) -> Svg msg
stage children =
    Svg.svg
        [ A.viewBox "-300 -300 600 600"
        , A.width "100%"
        , A.height "100%"
        ]
        children



-- HARMONOGRAPH ------------------------------------------------------------------------------------


harmonograph : Float -> HarmonographData -> Svg msg
harmonograph phase d =
    let
        count =
            max 2 d.samples

        -- The whole curve is traced over this many radians of t; the decay makes it spiral inward.
        tSpan =
            220.0

        xs =
            shiftPhase phase d.x

        ys =
            shiftPhase phase d.y

        point i =
            let
                t =
                    toFloat i / toFloat (count - 1) * tSpan
            in
            r2 (sumPend t xs) ++ "," ++ r2 (sumPend t ys)

        pts =
            String.join " " (List.map point (List.range 0 (count - 1)))
    in
    stage
        [ Svg.polyline
            [ A.points pts
            , A.fill "none"
            , A.stroke d.stroke
            , A.strokeWidth "1.1"
            , A.opacity "0.92"
            ]
            []
        ]


sumPend : Float -> List Scene.Pendulum -> Float
sumPend t ps =
    List.foldl (\p acc -> acc + p.amp * sin (p.freq * t + p.phase) * e ^ (-p.decay * t)) 0 ps


{-| Slowly precess every oscillator by the animation phase (a no-op at phase 0). -}
shiftPhase : Float -> List Scene.Pendulum -> List Scene.Pendulum
shiftPhase phase ps =
    List.map (\p -> { p | phase = p.phase + phase }) ps



-- ITERATED FUNCTION SYSTEM ------------------------------------------------------------------------


ifs : IfsData -> Svg msg
ifs d =
    let
        count =
            clamp 100 40000 d.points

        raw =
            chaosGame d.maps count

        ( cx, cy, scale ) =
            fitTransform raw

        dots =
            String.concat
                (List.map
                    (\( x, y ) ->
                        "M" ++ r1 ((x - cx) * scale) ++ " " ++ r1 (-(y - cy) * scale) ++ "h0.4"
                    )
                    raw
                )
    in
    stage
        [ Svg.path
            [ A.d dots
            , A.fill "none"
            , A.stroke d.stroke
            , A.strokeWidth "0.85"
            , A.strokeLinecap "round"
            , A.opacity "0.85"
            ]
            []
        ]


{-| The chaos game: from the origin, repeatedly pick a map (by its probability) and apply it,
collecting the visited points. Randomness is a small deterministic MINSTD generator seeded from a
constant, so the picture is stable frame to frame and needs no `elm/random`. -}
chaosGame : List Affine -> Int -> List ( Float, Float )
chaosGame maps count =
    let
        total =
            List.sum (List.map .p maps)

        cum =
            cumulative maps

        step _ ( seed, ( x, y ), acc ) =
            let
                seed2 =
                    modBy 2147483647 (48271 * seed)

                r =
                    toFloat seed2 / 2147483647 * total

                m =
                    Maybe.withDefault identityAffine (pick cum r)

                nx =
                    m.a * x + m.b * y + m.e

                ny =
                    m.c * x + m.d * y + m.f
            in
            ( seed2, ( nx, ny ), ( nx, ny ) :: acc )

        ( _, _, pts ) =
            List.foldl step ( 7919, ( 0, 0 ), [] ) (List.range 1 count)
    in
    -- drop the first few points while the orbit settles onto the attractor
    List.drop 10 (List.reverse pts)


identityAffine : Affine
identityAffine =
    { a = 1, b = 0, c = 0, d = 1, e = 0, f = 0, p = 1 }


cumulative : List Affine -> List ( Float, Affine )
cumulative maps =
    let
        go acc sofar ms =
            case ms of
                m :: rest ->
                    let
                        s =
                            sofar + m.p
                    in
                    go (( s, m ) :: acc) s rest

                [] ->
                    List.reverse acc
    in
    go [] 0 maps


pick : List ( Float, Affine ) -> Float -> Maybe Affine
pick cum r =
    case cum of
        ( thr, m ) :: rest ->
            if r <= thr || List.isEmpty rest then
                Just m

            else
                pick rest r

        [] ->
            Nothing


{-| Centre and uniform scale that fit a point cloud into the frame (with a margin), preserving aspect
ratio. Returns `(centreX, centreY, scale)`. -}
fitTransform : List ( Float, Float ) -> ( Float, Float, Float )
fitTransform pts =
    case pts of
        ( x0, y0 ) :: _ ->
            let
                bounds =
                    List.foldl
                        (\( x, y ) b ->
                            { minX = min b.minX x
                            , maxX = max b.maxX x
                            , minY = min b.minY y
                            , maxY = max b.maxY y
                            }
                        )
                        { minX = x0, maxX = x0, minY = y0, maxY = y0 }
                        pts

                w =
                    max 0.0001 (bounds.maxX - bounds.minX)

                h =
                    max 0.0001 (bounds.maxY - bounds.minY)
            in
            ( (bounds.minX + bounds.maxX) / 2
            , (bounds.minY + bounds.maxY) / 2
            , 560 / max w h
            )

        [] ->
            ( 0, 0, 1 )



-- POLYHEDRON --------------------------------------------------------------------------------------


polyhedron : Float -> PolyhedronData -> Svg msg
polyhedron phase d =
    let
        projected =
            Array.fromList (List.map (project (d.yaw + phase) d.pitch) d.vertices)

        edge ( i, j ) =
            case ( Array.get i projected, Array.get j projected ) of
                ( Just ( x1, y1 ), Just ( x2, y2 ) ) ->
                    Just
                        (Svg.line
                            [ A.x1 (r2 x1)
                            , A.y1 (r2 y1)
                            , A.x2 (r2 x2)
                            , A.y2 (r2 y2)
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
            Svg.circle
                [ A.cx (r2 x), A.cy (r2 y), A.r "3.2", A.fill d.stroke ]
                []
    in
    stage
        (List.filterMap edge d.edges
            ++ List.map dot (Array.toList projected)
        )


{-| Rotate a vertex by yaw (about the up axis) then pitch (about the screen-x axis), add a touch of
perspective, and project to screen coordinates (y down). -}
project : Float -> Float -> Vec3 -> ( Float, Float )
project yaw pitch v =
    let
        scale =
            120

        -- yaw about Y
        x1 =
            v.x * cos yaw + v.z * sin yaw

        z1 =
            -v.x * sin yaw + v.z * cos yaw

        -- pitch about X
        y2 =
            v.y * cos pitch - z1 * sin pitch

        z2 =
            v.y * sin pitch + z1 * cos pitch

        -- perspective foreshortening (camera 4 units back; denominator stays positive for |z| < 4)
        f =
            4 / (4 + z2)
    in
    ( x1 * scale * f, -y2 * scale * f )



-- LORENZ ATTRACTOR --------------------------------------------------------------------------------


lorenz : Float -> LorenzData -> Svg msg
lorenz phase d =
    let
        path3 =
            integrateLorenz d

        c =
            centroid path3

        yaw =
            d.yaw + phase

        proj =
            List.map (\p -> rotate2 yaw d.pitch { x = p.x - c.x, y = p.y - c.y, z = p.z - c.z }) path3

        ( cx, cy, scale ) =
            fitTransform proj

        toScreen ( x, y ) =
            r2 ((x - cx) * scale) ++ "," ++ r2 (-(y - cy) * scale)

        pts =
            String.join " " (List.map toScreen proj)
    in
    stage
        [ Svg.polyline
            [ A.points pts
            , A.fill "none"
            , A.stroke d.stroke
            , A.strokeWidth "1"
            , A.strokeLinecap "round"
            , A.opacity "0.9"
            ]
            []
        ]


{-| Euler-integrate the Lorenz system from a point just off the origin. -}
integrateLorenz : LorenzData -> List Vec3
integrateLorenz d =
    let
        steps =
            clamp 100 20000 d.steps

        step _ ( p, acc ) =
            let
                dx =
                    d.sigma * (p.y - p.x)

                dy =
                    p.x * (d.rho - p.z) - p.y

                dz =
                    p.x * p.y - d.beta * p.z

                np =
                    { x = p.x + dx * d.dt, y = p.y + dy * d.dt, z = p.z + dz * d.dt }
            in
            ( np, np :: acc )

        ( _, pts ) =
            List.foldl step ( { x = 0.1, y = 0, z = 0 }, [] ) (List.range 1 steps)
    in
    List.reverse pts


centroid : List Vec3 -> Vec3
centroid ps =
    let
        n =
            max 1 (List.length ps)

        sum =
            List.foldl (\p a -> { x = a.x + p.x, y = a.y + p.y, z = a.z + p.z }) { x = 0, y = 0, z = 0 } ps
    in
    { x = sum.x / toFloat n, y = sum.y / toFloat n, z = sum.z / toFloat n }


{-| Orthographic rotation by yaw (about Y) then pitch (about X), dropping z. -}
rotate2 : Float -> Float -> Vec3 -> ( Float, Float )
rotate2 yaw pitch v =
    let
        x1 =
            v.x * cos yaw + v.z * sin yaw

        z1 =
            -v.x * sin yaw + v.z * cos yaw

        y2 =
            v.y * cos pitch - z1 * sin pitch
    in
    ( x1, y2 )



-- FORCE-DIRECTED GRAPH ----------------------------------------------------------------------------


graph : GraphData -> Svg msg
graph d =
    let
        nN =
            List.length d.nodes

        positions =
            layout nN d.edges (clamp 1 400 d.iterations)

        ( cx, cy, scale ) =
            fitTransform positions

        place ( x, y ) =
            ( (x - cx) * scale, -(y - cy) * scale )

        placed =
            Array.fromList (List.map place positions)

        edgeLine ( a, b ) =
            case ( Array.get a placed, Array.get b placed ) of
                ( Just ( x1, y1 ), Just ( x2, y2 ) ) ->
                    Just
                        (Svg.line
                            [ A.x1 (r2 x1)
                            , A.y1 (r2 y1)
                            , A.x2 (r2 x2)
                            , A.y2 (r2 y2)
                            , A.stroke d.stroke
                            , A.strokeWidth "1.6"
                            , A.opacity "0.5"
                            ]
                            []
                        )

                _ ->
                    Nothing

        -- the JS backend has no <text> element, so a node is a filled dot with a brighter rim
        node ( x, y ) =
            Svg.circle
                [ A.cx (r2 x), A.cy (r2 y), A.r "9", A.fill d.stroke, A.opacity "0.95" ]
                []

        nodeSvgs =
            List.map node (Array.toList placed)
    in
    stage (List.filterMap edgeLine d.edges ++ nodeSvgs)


{-| A Fruchterman–Reingold spring layout: nodes start on a circle, then relax under edge attraction
and all-pairs repulsion, cooling each round. Returns a position per node (index-aligned). -}
layout : Int -> List ( Int, Int ) -> Int -> List ( Float, Float )
layout nN edges iters =
    let
        k =
            1.0

        init =
            List.map
                (\i ->
                    let
                        a =
                            2 * pi * toFloat i / toFloat (max 1 nN)
                    in
                    ( cos a, sin a )
                )
                (List.range 0 (nN - 1))

        go temp positions remaining =
            if remaining <= 0 then
                positions

            else
                go (temp * 0.95) (frStep edges k temp positions) (remaining - 1)
    in
    go (k * 0.4) init iters


frStep : List ( Int, Int ) -> Float -> Float -> List ( Float, Float ) -> List ( Float, Float )
frStep edges k temp positions =
    let
        arr =
            Array.fromList positions

        nN =
            Array.length arr

        getp i =
            Maybe.withDefault ( 0, 0 ) (Array.get i arr)

        -- start each node's displacement with repulsion from every other node
        repDisp =
            Array.fromList
                (List.map
                    (\i ->
                        List.foldl
                            (\j ( dx, dy ) ->
                                if j == i then
                                    ( dx, dy )

                                else
                                    let
                                        ( xi, yi ) =
                                            getp i

                                        ( xj, yj ) =
                                            getp j

                                        ux =
                                            xi - xj

                                        uy =
                                            yi - yj

                                        dist =
                                            sqrt (ux * ux + uy * uy) + 0.001

                                        force =
                                            k * k / dist
                                    in
                                    ( dx + ux / dist * force, dy + uy / dist * force )
                            )
                            ( 0, 0 )
                            (List.range 0 (nN - 1))
                    )
                    (List.range 0 (nN - 1))
                )

        -- then pull edge endpoints together
        disp =
            List.foldl
                (\( a, b ) acc ->
                    let
                        ( xa, ya ) =
                            getp a

                        ( xb, yb ) =
                            getp b

                        ux =
                            xa - xb

                        uy =
                            ya - yb

                        dist =
                            sqrt (ux * ux + uy * uy) + 0.001

                        force =
                            dist * dist / k

                        fx =
                            ux / dist * force

                        fy =
                            uy / dist * force

                        bump idx gx gy d2 =
                            case Array.get idx d2 of
                                Just ( dx, dy ) ->
                                    Array.set idx ( dx + gx, dy + gy ) d2

                                Nothing ->
                                    d2
                    in
                    bump b fx fy (bump a (-fx) (-fy) acc)
                )
                repDisp
                edges
    in
    List.indexedMap
        (\i ( x, y ) ->
            case Array.get i disp of
                Just ( dx, dy ) ->
                    let
                        d =
                            sqrt (dx * dx + dy * dy) + 0.0001

                        lim =
                            min d temp
                    in
                    ( x + dx / d * lim, y + dy / d * lim )

                Nothing ->
                    ( x, y )
        )
        positions



-- CELLULAR AUTOMATON ------------------------------------------------------------------------------


automaton : AutomatonData -> Svg msg
automaton d =
    let
        w =
            clamp 5 251 d.width

        gens =
            clamp 1 200 d.generations

        center =
            w // 2

        row0 =
            Array.fromList
                (List.map (\col -> List.member (col - center) d.seed) (List.range 0 (w - 1)))

        rows =
            buildRows d.rule gens row0

        cell =
            580 / toFloat (max w gens)

        ox =
            -(toFloat w * cell) / 2

        oy =
            -(toFloat gens * cell) / 2

        squares =
            String.concat
                (List.concat
                    (List.indexedMap
                        (\g rowArr ->
                            List.filterMap
                                (\col ->
                                    if Maybe.withDefault False (Array.get col rowArr) then
                                        Just (square (ox + toFloat col * cell) (oy + toFloat g * cell) cell)

                                    else
                                        Nothing
                                )
                                (List.range 0 (w - 1))
                        )
                        rows
                    )
                )
    in
    stage [ Svg.path [ A.d squares, A.fill d.stroke, A.opacity "0.95" ] [] ]


square : Float -> Float -> Float -> String
square x y s =
    "M" ++ r1 x ++ " " ++ r1 y ++ "h" ++ r1 s ++ "v" ++ r1 s ++ "h" ++ r1 (-s) ++ "z"


{-| Evolve `gens` rows from `row0` under the elementary rule. -}
buildRows : Int -> Int -> Array Bool -> List (Array Bool)
buildRows rule gens row0 =
    let
        go remaining cur acc =
            if remaining <= 0 then
                List.reverse acc

            else
                go (remaining - 1) (nextRow rule cur) (cur :: acc)
    in
    go gens row0 []


nextRow : Int -> Array Bool -> Array Bool
nextRow rule cur =
    let
        w =
            Array.length cur

        bitAt i =
            if Maybe.withDefault False (Array.get i cur) then
                1

            else
                0
    in
    Array.fromList
        (List.map
            (\i -> ruleBit rule (4 * bitAt (i - 1) + 2 * bitAt i + bitAt (i + 1)))
            (List.range 0 (w - 1))
        )


ruleBit : Int -> Int -> Bool
ruleBit rule idx =
    modBy 2 (rule // (2 ^ idx)) == 1



-- NUMBER FORMATTING -------------------------------------------------------------------------------


{-| Round to 2 / 1 decimals to keep SVG coordinate strings short. -}
r2 : Float -> String
r2 x =
    String.fromFloat (toFloat (round (x * 100)) / 100)


r1 : Float -> String
r1 x =
    String.fromFloat (toFloat (round (x * 10)) / 10)
