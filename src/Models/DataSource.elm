module Models.DataSource exposing (..)

import Json.Decode exposing (Decoder, field, float, int, list, map2, string, map5)
import Json.Encode exposing (Value)

type alias DataSource =
    { host : String
    , portNumber : Int
    , username : String
    , password : String
    , database : String
    }


dataSourceDecoder : Decoder DataSource
dataSourceDecoder =
    map5 DataSource
        (field "host" string)
        (field "port" int)
        (field "username" string)
        (field "password" string)
        (field "database" string)


dataSourceEncoder : DataSource -> Value
dataSourceEncoder dataSource =
    Json.Encode.object
        [ ( "host", Json.Encode.string dataSource.host )
        , ( "port", Json.Encode.int dataSource.portNumber )
        , ( "username", Json.Encode.string dataSource.username )
        , ( "password", Json.Encode.string dataSource.password )
        , ( "database", Json.Encode.string dataSource.database )
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


tableColumnDecoder : Decoder TableColumn
tableColumnDecoder =
    map2 TableColumn
        (field "name" string)
        (field "dataType" string)


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
