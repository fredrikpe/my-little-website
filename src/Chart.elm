module Chart exposing (viewDataset)

import Color
import Dataset
import Html
import LineChart as LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Axis.Line as AxisLine
import LineChart.Axis.Range as Range
import LineChart.Axis.Tick as Tick
import LineChart.Axis.Ticks as Ticks
import LineChart.Axis.Title as Title
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk exposing (..)
import LineChart.Legends as Legends
import LineChart.Line as Line
import Svg exposing (Attribute, Svg, g, text_, tspan)
import Time
import Util


type alias Data =
    Dataset.Point


viewDataset : Dataset.Dataset -> (Maybe Data -> msg) -> Maybe Data -> Html.Html msg
viewDataset dataset msg hovered =
    let
        rcharts =
            Dataset.makeCharts dataset

        toFloat =
            Dataset.dateConverter dataset

        toString =
            \x -> "string"
    in
    case rcharts of
        Ok charts ->
            viewChart (Maybe.withDefault [] (List.head charts)) toFloat toString msg hovered

        Err e ->
            Html.text e


viewChart chart toFloat toString msg hovered =
    LineChart.viewCustom
        { y = Axis.default 450 "Weight" .y
        , x = xAxisConfig toFloat toString
        , container = Container.styled "line-chart-1" [ ( "font-family", "monospace" ) ]
        , interpolation = Interpolation.default
        , intersection = Intersection.default
        , legends = Legends.default
        , events = Events.hoverOne msg
        , junk =
            Junk.hoverOne hovered
                [ ( "Age", Debug.toString << .x )
                , ( "Weight", Debug.toString << .y )
                ]
        , grid = Grid.default
        , area = Area.default
        , line = Line.default
        , dots = Dots.hoverOne hovered
        }
        (List.map3
            (\c color name ->
                LineChart.line color Dots.none name c.points
            )
            chart
            colors
            names
        )


xAxisConfig : (String -> Float) -> (Float -> String) -> Axis.Config Data msg
xAxisConfig toFloat toString =
    Axis.custom
        { title = Title.default "Year"
        , variable = Just << (\point -> toFloat point.x)
        , pixels = 700
        , range = Range.padded 20 20
        , axisLine = AxisLine.full Color.black
        , ticks = Ticks.floatCustom 7 (customTick toString)
        }


customTick : (Float -> String) -> Float -> Tick.Config msg
customTick toString number =
    let
        label =
            Junk.label Color.black (toString number)
    in
    Tick.custom
        { position = number
        , color = Color.black
        , width = 1
        , length = 7
        , grid = True
        , direction = Tick.positive
        , label = Just label
        }


colors =
    [ Color.red
    , Color.orange
    , Color.yellow
    , Color.green
    , Color.blue
    , Color.purple
    , Color.brown
    , Color.lightRed
    , Color.lightOrange
    , Color.lightYellow
    , Color.lightGreen
    , Color.lightBlue
    , Color.lightPurple
    , Color.lightBrown
    ]


names =
    [ "a", "b", "c", "d", "e", "f", "g" ]
