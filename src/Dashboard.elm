module Dashboard exposing (..)

import Json.Decode exposing (Decoder, field, int, string, nullable, list, map2, map4, map5, float, andThen, map, map3)
import Json.Encode exposing (Value)
import Url.Parser exposing (Parser, custom)

type alias Dashboard = 
  { dashboard_id : DashboardId
  , title : String
  , widgets : List Widget
  , dataSource : DataSource
  }

type DashboardId 
  = DashboardId Int

type Widget 
  = Pie { widget_id : Int, title: String, table: String, data_column : String }
  | Histogram { widget_id : Int, title: String, table: String,  data_column : String }

type alias DataSource = 
  { host : String
  , portNumber : Int
  , username : String
  , password : String
  , database : String
  }

dashboardsDecoder : Decoder (List Dashboard)
dashboardsDecoder =
  list dashboardDecoder

dashboardDecoder : Decoder Dashboard
dashboardDecoder =
  map4 Dashboard
    (field "dashboard_id" idDecoder)
    (field "title" string)
    (field "widgets" (list widgetDecoder))
    (field "dataSource" (dataSourceDecoder))

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


widgetDecoder : Decoder Widget
widgetDecoder =
  field "widget_type" string
    |> andThen
        (\widget_type ->
          case widget_type of
            "PieChart" ->
              Json.Decode.map Pie
                (map4 (\widget_id title table data_column ->
                    { widget_id = widget_id 
                    , title = title
                    , table = table
                    , data_column = data_column
                    }
                 )
                 (field "widget_id" int)
                 (field "title" string)
                 (field "table" string)
                 (field "data_column" string)
                )
                
            "Histogram" ->
              Json.Decode.map Histogram
                (map4 (\widget_id title table data_column ->
                    { widget_id = widget_id
                    , title = title
                    , table = table
                    , data_column = data_column
                    }
                 )
                 (field "widget_id" int)
                 (field "title" string)
                 (field "table" string)
                 (field "data_column" string)
                )
        
            _ ->
              Json.Decode.fail ("Unknown widget type: " ++ widget_type)
        )


dataSourceDecoder : Decoder DataSource
dataSourceDecoder =
  map5 DataSource
    (field "host" string)
    (field "port" int)
    (field "username" string)
    (field "password" string)
    (field "database" string)

  
dashboardEncoder : Dashboard -> Value
dashboardEncoder dashboard =
  Json.Encode.object
    [ ("dashboard_id", idEncoder dashboard.dashboard_id)
    , ("title", Json.Encode.string dashboard.title)
    , ("widgets", Json.Encode.list widgetEncoder dashboard.widgets)
    , ("dataSource", dataSourceEncoder dashboard.dataSource)
    ]
  

widgetEncoder : Widget -> Value
widgetEncoder widget =
  case widget of
    Pie { widget_id, title, table, data_column } ->
      Json.Encode.object
        [ ("widget_type", Json.Encode.string "PieChart")
        , ("widget_id", Json.Encode.int widget_id)
        , ("title", Json.Encode.string title)
        , ("table", Json.Encode.string table)
        , ("data_column", Json.Encode.string data_column)

        ]
        
    Histogram { widget_id, title, table, data_column } ->
      Json.Encode.object
        [ ("widget_type", Json.Encode.string "Histogram")
        , ("widget_id", Json.Encode.int widget_id)
        , ("title", Json.Encode.string title)
        , ("table", Json.Encode.string table)
        , ("data_column", Json.Encode.string data_column)
        ]
        

dataSourceEncoder : DataSource -> Value
dataSourceEncoder dataSource =
  Json.Encode.object
    [ ("host", Json.Encode.string dataSource.host)
    , ("port", Json.Encode.int dataSource.portNumber)
    , ("username", Json.Encode.string dataSource.username)
    , ("password", Json.Encode.string dataSource.password)
    , ("database", Json.Encode.string dataSource.database)
    ]

type WidgetData 
    = Piedata (List PieData)
    | Histogramdata (List Float)

type alias PieData =
    { title : String
    , count : Int
    }


pieDataDecoder : Decoder (List PieData)
pieDataDecoder =
    field "data" (list (map2 PieData (field "title" string) (field "count" int)))


histogramDataDecoder : Decoder (List Float)
histogramDataDecoder =
    field "data" (list float)