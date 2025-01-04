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
    (field "dataSource" (nullable dataSourceDecoder))


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

