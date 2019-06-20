module Chart exposing (viewDataset)

import Color
import Dataset
import Html
import LineChart as LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk exposing (..)
import LineChart.Legends as Legends
import LineChart.Line as Line
import Svg exposing (Attribute, Svg, g, text_, tspan)


type alias Data =
    Dataset.Point


viewDataset : Dataset.Dataset -> (Maybe Data -> msg) -> Maybe Data -> Html.Html msg
viewDataset dataset msg hovered =
    let
        charts =
            Dataset.iterator dataset
    in
    LineChart.viewCustom
        { y = Axis.default 450 "Weight" .y
        , x = Axis.default 700 "Age" .x
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
            (Maybe.withDefault [] (List.head charts))
            colors
            names
        )


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
