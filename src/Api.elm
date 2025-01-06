module Api exposing (..)

import Http
import Json.Decode exposing (list)

import Types exposing (..)
import Decoders exposing (..)
import Message exposing (..)

get_dashboards : Cmd Msg
get_dashboards =
  Http.post
    { url = "http://127.0.0.1:6969/get-dashboards"
    , body = Http.emptyBody
    , expect = Http.expectJson GotDashboards (Json.Decode.oneOf [list dashboardDecoder, Json.Decode.succeed []])
    }

create_dashboard : Dashboard -> Cmd Msg
create_dashboard dashboard =
  Http.post
    { url = "http://127.0.0.1:6969/create-or-update-dashboard"
    , body = Http.jsonBody 
        (dashboardEncoder dashboard )
        
    , expect = Http.expectJson NavigateToDashboard dashboardDecoder
    }

update_dashboard : Dashboard -> Cmd Msg
update_dashboard dashboard =
  Http.post
    { url = "http://127.0.0.1:6969/create-or-update-dashboard"
    , body = Http.jsonBody 
        (dashboardEncoder dashboard )
        
    , expect = Http.expectJson UpdateCurrentDashboard dashboardDecoder
    }
