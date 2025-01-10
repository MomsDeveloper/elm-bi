module Models.Widgets exposing (..)

import Json.Decode exposing (Decoder, andThen, field, int, map5, string)
import Json.Encode exposing (Value)
import Models.DataSource exposing (..)


type Widget
    = Pie { widget_id : Int, title : String, table : String, data_column : String, data : List PieData }
    | Histogram { widget_id : Int, title : String, table : String, data_column : String, data : List HistogramData }


widgetDecoder : Decoder Widget
widgetDecoder =
    field "widget_type" string
        |> andThen
            (\widget_type ->
                case widget_type of
                    "PieChart" ->
                        Json.Decode.map Pie
                            (map5
                                (\widget_id title table data_column data ->
                                    { widget_id = widget_id
                                    , title = title
                                    , table = table
                                    , data_column = data_column
                                    , data = data
                                    }
                                )
                                (field "widget_id" int)
                                (field "title" string)
                                (field "table" string)
                                (field "data_column" string)
                                (field "data" pieDataDecoder)
                            )

                    "Histogram" ->
                        Json.Decode.map Histogram
                            (map5
                                (\widget_id title table data_column data ->
                                    { widget_id = widget_id
                                    , title = title
                                    , table = table
                                    , data_column = data_column
                                    , data = data
                                    }
                                )
                                (field "widget_id" int)
                                (field "title" string)
                                (field "table" string)
                                (field "data_column" string)
                                (field "data" histogramDataDecoder)
                            )

                    _ ->
                        Json.Decode.fail ("Unknown widget type: " ++ widget_type)
            )


widgetEncoder : Widget -> Value
widgetEncoder widget =
    case widget of
        Pie { widget_id, title, table, data_column, data } ->
            Json.Encode.object
                [ ( "widget_type", Json.Encode.string "PieChart" )
                , ( "widget_id", Json.Encode.int widget_id )
                , ( "title", Json.Encode.string title )
                , ( "table", Json.Encode.string table )
                , ( "data_column", Json.Encode.string data_column )
                , ( "data", pieDataEncoder data )
                ]

        Histogram { widget_id, title, table, data_column, data } ->
            Json.Encode.object
                [ ( "widget_type", Json.Encode.string "Histogram" )
                , ( "widget_id", Json.Encode.int widget_id )
                , ( "title", Json.Encode.string title )
                , ( "table", Json.Encode.string table )
                , ( "data_column", Json.Encode.string data_column )
                , ( "data", histogramDataEncoder data )
                ]
