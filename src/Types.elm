module Types exposing (..)

type alias PieSection = 
  { title : String
  , percentage : Float
  }

type alias Bin = 
  { range : {
    left : Maybe Float
    , right : Maybe Float
  }
  , value : Float
 }

type Widget 
  = Pie { title: String, table: String, x_column : String , sections : Maybe (List PieSection) }
  | Histogram { title: String, table: String,  x_column : String , sections : Maybe (List Bin) }


type alias DataSource = 
  { host : String
  , portNumber : Int
  , username : String
  , password : String
  , database : String
  }

type alias Dashboard = 
  { dashboard_id : Int
  , title : String
  , widgets : List Widget
  , dataSource : DataSource
  }

type alias TableColumn = 
  { name : String
  , dataType : String
  }

type alias Table = 
  {
    name : String
    , columns : List TableColumn
  }

type alias Error = 
  { detail : String }