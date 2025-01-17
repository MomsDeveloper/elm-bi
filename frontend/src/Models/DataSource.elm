module Models.DataSource exposing (DataSource, HistogramData, PieData, Table, TableColumn, WidgetData(..), dataSourceDecoder, dataSourceEncoder, histogramDataDecoder, histogramDataEncoder, pieDataDecoder, pieDataEncoder, tableColumnDecoder, tableColumnEncoder, tableDecoder, tableEncoder, tablesDecoder)

import Json.Decode exposing (Decoder, field, float, int, list, map, map2, map6, string)
import Json.Encode exposing (Value)


type alias DataSource =
    { host : String
    , portNumber : Int
    , username : String
    , password : String
    , database : String
    , tables : List Table
    }


dataSourceDecoder : Decoder DataSource
dataSourceDecoder =
    map6 DataSource
        (field "host" string)
        (field "port" int)
        (field "username" string)
        (field "password" string)
        (field "database" string)
        (field "tables" tablesDecoder)


dataSourceEncoder : DataSource -> Value
dataSourceEncoder dataSource =
    Json.Encode.object
        [ ( "host", Json.Encode.string dataSource.host )
        , ( "port", Json.Encode.int dataSource.portNumber )
        , ( "username", Json.Encode.string dataSource.username )
        , ( "password", Json.Encode.string dataSource.password )
        , ( "database", Json.Encode.string dataSource.database )
        , ( "tables", Json.Encode.list tableEncoder dataSource.tables )
        ]


type alias Table =
    { name : String
    , columns : List TableColumn
    }


type alias TableColumn =
    { name : String
    , dataType : String
    }


tablesDecoder : Decoder (List Table)
tablesDecoder =
    list tableDecoder


tableDecoder : Decoder Table
tableDecoder =
    map2 Table
        (field "name" string)
        (field "columns" (list tableColumnDecoder))


tableEncoder : Table -> Value
tableEncoder table =
    Json.Encode.object
        [ ( "name", Json.Encode.string table.name )
        , ( "columns", Json.Encode.list tableColumnEncoder table.columns )
        ]


tableColumnDecoder : Decoder TableColumn
tableColumnDecoder =
    map2 TableColumn
        (field "name" string)
        (field "dataType" string)


tableColumnEncoder : TableColumn -> Value
tableColumnEncoder tableColumn =
    Json.Encode.object
        [ ( "name", Json.Encode.string tableColumn.name )
        , ( "dataType", Json.Encode.string tableColumn.dataType )
        ]


type WidgetData
    = Piedata (List PieData)
    | Histogramdata (List HistogramData)


type alias PieData =
    { title : String
    , count : Int
    }


type alias HistogramData =
    { data : Float
    }


pieDataDecoder : Decoder (List PieData)
pieDataDecoder =
    list (map2 PieData (field "title" string) (field "count" int))


pieDataEncoder : List PieData -> Value
pieDataEncoder pieData =
    Json.Encode.list pieDataToValue pieData


pieDataToValue : PieData -> Value
pieDataToValue pieData =
    Json.Encode.object
        [ ( "title", Json.Encode.string pieData.title )
        , ( "count", Json.Encode.int pieData.count )
        ]


histogramDataDecoder : Decoder (List HistogramData)
histogramDataDecoder =
    list (map HistogramData float)


histogramDataEncoder : List HistogramData -> Value
histogramDataEncoder histogramData =
    Json.Encode.list histogramDataToValue histogramData


histogramDataToValue : HistogramData -> Value
histogramDataToValue histogramData =
    Json.Encode.object
        [ ( "data", Json.Encode.float histogramData.data )
        ]
