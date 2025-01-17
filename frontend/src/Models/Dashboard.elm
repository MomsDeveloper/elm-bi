module Models.Dashboard exposing (Dashboard, DashboardId(..), dashboardDecoder, dashboardEncoder, dashboardsDecoder, idDecoder, idEncoder, idParser, idToString)

import Json.Decode exposing (Decoder, field, int, list, map, map4, string)
import Json.Encode exposing (Value)
import Models.DataSource exposing (DataSource, dataSourceDecoder, dataSourceEncoder)
import Models.Widgets exposing (Widget, widgetDecoder, widgetEncoder)
import Url.Parser exposing (Parser, custom)


type alias Dashboard =
    { dashboard_id : DashboardId
    , title : String
    , widgets : List Widget
    , dataSource : DataSource
    }


type DashboardId
    = DashboardId Int


dashboardsDecoder : Decoder (List Dashboard)
dashboardsDecoder =
    list dashboardDecoder


dashboardDecoder : Decoder Dashboard
dashboardDecoder =
    map4 Dashboard
        (field "dashboard_id" idDecoder)
        (field "title" string)
        (field "widgets" (list widgetDecoder))
        (field "dataSource" dataSourceDecoder)


dashboardEncoder : Dashboard -> Value
dashboardEncoder dashboard =
    Json.Encode.object
        [ ( "dashboard_id", idEncoder dashboard.dashboard_id )
        , ( "title", Json.Encode.string dashboard.title )
        , ( "widgets", Json.Encode.list widgetEncoder dashboard.widgets )
        , ( "dataSource", dataSourceEncoder dashboard.dataSource )
        ]


idToString : DashboardId -> String
idToString (DashboardId id) =
    String.fromInt id


idDecoder : Decoder DashboardId
idDecoder =
    map DashboardId int


idEncoder : DashboardId -> Value
idEncoder (DashboardId id) =
    Json.Encode.int id


idParser : Parser (DashboardId -> a) a
idParser =
    custom "DASHBOARDID" <|
        \dashboardId ->
            Maybe.map DashboardId (String.toInt dashboardId)
