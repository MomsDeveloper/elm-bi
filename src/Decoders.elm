module Decoders exposing (..)

import Json.Decode exposing (Decoder, field, int, string, nullable, list, map2, map4, map5, float, andThen)
import Json.Encode exposing (Value)
import Types exposing (..)

dashboardDecoder : Decoder Dashboard
dashboardDecoder =
  map4 Dashboard
    (field "dashboard_id" int)
    (field "title" string)
    (field "widgets" (list widgetDecoder))
    (field "dataSource" (dataSourceDecoder))


widgetDecoder : Decoder Widget
widgetDecoder =
  field "widget_type" string
    |> andThen
        (\widget_type ->
          case widget_type of
            "PieChart" ->
              Json.Decode.map Pie
                (map4 (\title table x_column sections ->
                    { title = title
                    , table = table
                    , x_column = x_column
                    , sections = sections
                    }
                 )
                 (field "title" string)
                 (field "table" string)
                 (field "x_column" string)
                 (field "sections" (nullable (list pieSectionDecoder)))
                )
                
            "Histogram" ->
              Json.Decode.map Histogram
                (map4 (\title table x_column bins ->
                    { title = title
                    , table = table
                    , x_column = x_column
                    , sections = bins
                    }
                 )
                 (field "title" string)
                 (field "table" string)
                 (field "x_column" string)
                 (field "bins" (nullable (list binDecoder)))
                )
        
            _ ->
              Json.Decode.fail ("Unknown widget type: " ++ widget_type)
        )


pieSectionDecoder : Decoder PieSection
pieSectionDecoder =
  map2 PieSection
    (field "title" string)
    (field "percentage" float)

binDecoder : Decoder Bin
binDecoder =
  map2 Bin
    (field "range"
      (map2 (\left right -> { left = left, right = right })
        (field "left" (nullable float))
        (field "right" (nullable float))
      )
    )
    (field "value" float)


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
    [ ("dashboard_id", Json.Encode.int dashboard.dashboard_id)
    , ("title", Json.Encode.string dashboard.title)
    , ("widgets", Json.Encode.list widgetEncoder dashboard.widgets)
    , ("dataSource", dataSourceEncoder dashboard.dataSource)
    ]

widgetEncoder : Widget -> Value
widgetEncoder widget =
  case widget of
    Pie { title, table, x_column, sections } ->
      Json.Encode.object
        [ ("widget_type", Json.Encode.string "PieChart")
        , ("title", Json.Encode.string title)
        , ("table", Json.Encode.string table)
        , ("x_column", Json.Encode.string x_column)
        , ("sections", case sections of
            Nothing -> Json.Encode.null
            Just s -> Json.Encode.list pieSectionEncoder s
          )
        ]
        
    Histogram { title, table, x_column, sections } ->
      Json.Encode.object
        [ ("widget_type", Json.Encode.string "Histogram")
        , ("title", Json.Encode.string title)
        , ("table", Json.Encode.string table)
        , ("x_column", Json.Encode.string x_column)
        , ("bins", case sections of
            Nothing -> Json.Encode.null
            Just s -> Json.Encode.list binEncoder s
          )
        ]
        
pieSectionEncoder : PieSection -> Value
pieSectionEncoder pieSection =
  Json.Encode.object
    [ ("title", Json.Encode.string pieSection.title)
    , ("percentage", Json.Encode.float pieSection.percentage)
    ]

binEncoder : Bin -> Value
binEncoder bin =
  Json.Encode.object
    [ ("range", Json.Encode.object
        [ ("left", case bin.range.left of
            Nothing -> Json.Encode.null
            Just value -> Json.Encode.float value
          )
        , ("right", case bin.range.right of
            Nothing -> Json.Encode.null
            Just value -> Json.Encode.float value
          )
        ]
      )
    , ("value", Json.Encode.float bin.value)
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

tableDecoder : Decoder Table
tableDecoder =
  map2 Table
    (field "name" string)
    (field "columns" (list tableColumnDecoder))

  
tableColumnDecoder : Decoder TableColumn
tableColumnDecoder =
  map2 TableColumn
    (field "name" string)
    (field "dataType" string)
