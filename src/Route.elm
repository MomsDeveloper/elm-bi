module Route exposing (Route(..), parseUrl, pushUrl)

import Url exposing (Url)
import Url.Parser exposing (..)
import Dashboard exposing (DashboardId)
import Browser.Navigation as Nav

type Route
    = NotFound
    | Dashboards
    | EditDashboard DashboardId
    | Dashboard DashboardId


parseUrl : Url -> Route
parseUrl url =
    case parse matchRoute url of
        Just route ->
            route

        Nothing ->
            NotFound

matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ map Dashboards top
        , map Dashboards (s "dashboards")
        , map EditDashboard (s "dashboards" </> Dashboard.idParser </> s "edit")
        , map Dashboard (s "dashboards" </> Dashboard.idParser)
        ]

pushUrl : Route -> Nav.Key -> Cmd msg
pushUrl route navKey =
    routeToString route
        |> Nav.pushUrl navKey


routeToString : Route -> String
routeToString route =
    case route of
        NotFound ->
            "/not-found"

        Dashboards ->
            "/dashboards"

        EditDashboard dashboardId ->
            "/dashboards/" ++ Dashboard.idToString dashboardId ++ "/edit" 

        Dashboard dashboardId ->
            "/dashboards/" ++ Dashboard.idToString dashboardId

