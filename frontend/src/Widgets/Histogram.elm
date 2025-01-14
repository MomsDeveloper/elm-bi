module Widgets.Histogram exposing (view)

import Axis
import Color
import Histogram exposing (Bin)
import Models.DataSource exposing (HistogramData)
import Scale exposing (ContinuousScale)
import TypedSvg exposing (g, rect, svg)
import TypedSvg.Attributes exposing (class, fill, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), Transform(..))


histogram : Float -> Float -> List Float -> List (Bin Float Float)
histogram minX maxX model =
    Histogram.float
        |> Histogram.withDomain ( minX, maxX + 1 )
        |> Histogram.compute model


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


xScale : Float -> Float -> ContinuousScale Float
xScale minX maxX =
    Scale.linear ( 0, w - 2 * padding ) ( minX, maxX + 1 )


yScaleFromBins : List (Bin Float Float) -> ContinuousScale Float
yScaleFromBins bins =
    List.map .length bins
        |> List.maximum
        |> Maybe.withDefault 0
        |> toFloat
        |> Tuple.pair 0
        |> Scale.linear ( h - 2 * padding, 0 )


xAxis : ContinuousScale Float -> Svg msg
xAxis scale =
    Axis.bottom [] scale


yAxis : List (Bin Float Float) -> Svg msg
yAxis bins =
    Axis.left [ Axis.tickCount 5 ] (yScaleFromBins bins)


column : ContinuousScale Float -> ContinuousScale Float -> Bin Float Float -> Svg msg
column xScaleColumn yScale { length, x0, x1 } =
    rect
        [ x <| Scale.convert xScaleColumn x0
        , y <| Scale.convert yScale (toFloat length)
        , width <| Scale.convert xScaleColumn x1 - Scale.convert xScaleColumn x0
        , height <| h - Scale.convert yScale (toFloat length) - 2 * padding
        , fill <| Paint <| Color.rgb255 46 118 149
        ]
        []


view : List HistogramData -> Svg msg
view model =
    let
        data =
            List.map .data model

        minX =
            List.minimum data |> Maybe.withDefault 0

        maxX =
            List.maximum data |> Maybe.withDefault 20

        bins =
            histogram minX maxX data

        xScaleDynamic =
            xScale minX maxX
    in
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis xScaleDynamic ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis bins ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ] <|
            List.map (column xScaleDynamic (yScaleFromBins bins)) bins
        ]
