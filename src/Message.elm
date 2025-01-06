module Message exposing (..)

import Browser
import Url exposing (Url)
import Browser.Navigation as Browser
import Http
import Types exposing (..)


type Msg 
    = GotDashboards (Result Http.Error (List Dashboard))
    | UpdateCurrentDashboard (Result Http.Error Dashboard)
    | CreateDashboard
    | NewPassword String
    | NewUsername String
    | NewHost String
    | NewPort String
    | NewDatabase String
    | NewTitle String
    | NavigateToDashboard (Result Http.Error Dashboard)
    | ShowForm
    | CloseForm
    | NavigateToHome
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest