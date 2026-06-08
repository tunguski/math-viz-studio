module Draw exposing
    ( Vec3
    , stage
    , r1, r2
    , fitTransform
    , project, rotate2, centroid
    , curve
    )

{-| Shared **visualisation primitives** — the SVG drawing helpers every visualisation reaches for, so
the per-visualisation modules only contain what is specific to them.

  - `stage` — the common centred square frame.
  - `r1` / `r2` — short coordinate strings.
  - `fitTransform` — centre-and-scale a 2-D point cloud into the frame.
  - `project` / `rotate2` / `centroid` — 3-D → 2-D for the polyhedron and Lorenz attractor.

@docs Vec3
@docs stage
@docs r1, r2
@docs fitTransform
@docs project, rotate2, centroid

-}

import Array exposing (Array)
import Color exposing (Coloring)
import Svg exposing (Svg)
import Svg.Attributes as A


{-| A point in 3-space (a vertex, a trajectory sample). -}
type alias Vec3 =
    { x : Float, y : Float, z : Float }


{-| Draw a curve through `pts` with a `Coloring`. A `Uniform` coloring is one cheap `<polyline>`; a
`Varying` one is split into a couple of dozen bands (each a `<polyline>` sharing its endpoints with
its neighbours, so the line stays continuous) so the colour can sweep along the curve. -}
curve : String -> Coloring -> Float -> List ( Float, Float ) -> Svg msg
curve width coloring phase pts =
    case coloring of
        Color.Uniform f ->
            polyline width (f phase) pts

        Color.Varying f ->
            let
                arr =
                    Array.fromList pts

                n =
                    Array.length arr

                bands =
                    clamp 1 24 (n // 2)

                per =
                    max 1 (n // bands)

                band b =
                    let
                        start =
                            b * per

                        end =
                            if b == bands - 1 then
                                n

                            else
                                (b + 1) * per + 1

                        t =
                            toFloat b / toFloat (max 1 (bands - 1))
                    in
                    polyline width (f t phase) (Array.toList (Array.slice start end arr))
            in
            Svg.g [] (List.map band (List.range 0 (bands - 1)))


polyline : String -> String -> List ( Float, Float ) -> Svg msg
polyline width color pts =
    Svg.polyline
        [ A.points (String.join " " (List.map (\( x, y ) -> r2 x ++ "," ++ r2 y) pts))
        , A.fill "none"
        , A.stroke color
        , A.strokeWidth width
        , A.strokeLinejoin "round"
        , A.strokeLinecap "round"
        , A.opacity "0.92"
        ]
        []


{-| The shared frame: a centred 600×600 viewBox the renderers draw into (origin in the middle, the
unit a pixel). -}
stage : List (Svg msg) -> Svg msg
stage children =
    Svg.svg
        [ A.viewBox "-300 -300 600 600"
        , A.width "100%"
        , A.height "100%"
        ]
        children


{-| Round to 2 / 1 decimals to keep SVG coordinate strings short. -}
r2 : Float -> String
r2 x =
    String.fromFloat (toFloat (round (x * 100)) / 100)


r1 : Float -> String
r1 x =
    String.fromFloat (toFloat (round (x * 10)) / 10)


{-| Centre and uniform scale that fit a 2-D point cloud into the frame (with a margin), preserving
aspect ratio. Returns `( centreX, centreY, scale )`. -}
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


{-| Rotate a vertex by yaw (about the up axis) then pitch (about the screen-x axis), add a touch of
perspective, and project to screen coordinates (y down). -}
project : Float -> Float -> Vec3 -> ( Float, Float )
project yaw pitch v =
    let
        scale =
            120

        x1 =
            v.x * cos yaw + v.z * sin yaw

        z1 =
            -v.x * sin yaw + v.z * cos yaw

        y2 =
            v.y * cos pitch - z1 * sin pitch

        z2 =
            v.y * sin pitch + z1 * cos pitch

        -- perspective foreshortening (camera 4 units back; denominator stays positive for |z| < 4)
        f =
            4 / (4 + z2)
    in
    ( x1 * scale * f, -y2 * scale * f )


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


{-| The mean of a list of points. -}
centroid : List Vec3 -> Vec3
centroid ps =
    let
        n =
            max 1 (List.length ps)

        sum =
            List.foldl (\p a -> { x = a.x + p.x, y = a.y + p.y, z = a.z + p.z }) { x = 0, y = 0, z = 0 } ps
    in
    { x = sum.x / toFloat n, y = sum.y / toFloat n, z = sum.z / toFloat n }
