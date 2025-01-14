module Widgets.PieChart exposing (view)

import Array exposing (Array)
import Color exposing (Color)
import Models.DataSource
import Path
import Shape exposing (defaultPieConfig)
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (fill, fontSize, stroke, textAnchor, transform, viewBox)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Paint(..), Transform(..), em)


size : Float
size =
    100


colors : Array Color
colors =
    Array.fromList
        [ Color.rgb255 152 171 198
        , Color.rgb255 138 137 166
        , Color.rgb255 123 104 136
        , Color.rgb255 107 72 107
        , Color.rgb255 159 92 85
        , Color.rgb255 208 116 60
        , Color.rgb255 255 96 0
        ]


radius : Float
radius =
    size / 2 - 20


pieSlice : Int -> Shape.Arc -> Svg msg
pieSlice index datum =
    Path.element (Shape.arc datum) [ fill <| Paint <| Maybe.withDefault Color.black <| Array.get index colors, stroke <| Paint Color.white ]


pieLabel : Shape.Arc -> ( String, Float ) -> Svg msg
pieLabel slice ( label, _ ) =
    let
        ( x, y ) =
            Shape.centroid { slice | innerRadius = radius + 5, outerRadius = radius + 5 }
    in
    text_
        [ transform [ Translate x y ]
        , textAnchor AnchorMiddle
        , fontSize (em 0.4)
        ]
        [ text label ]


view : List Models.DataSource.PieData -> Svg msg
view data =
    let
        model =
            data |> List.map (\datum -> ( datum.title, toFloat datum.count ))

        pieData =
            model |> List.map Tuple.second |> Shape.pie { defaultPieConfig | outerRadius = radius }
    in
    svg [ viewBox 0 0 size size ]
        [ g [ transform [ Translate (size / 2) (size / 2) ] ]
            [ g [] <| List.indexedMap pieSlice pieData
            , g [] <| List.map2 pieLabel pieData model
            ]
        ]
