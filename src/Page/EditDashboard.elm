module Page.EditDashboard exposing (..)

import Browser.Navigation as Nav
import Dashboard exposing (..)
import RemoteData exposing (WebData)
import Http exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Route
import Error exposing (buildErrorMessage)
import Json.Decode exposing (field)
import Json.Encode exposing (object)


type alias Model =
    { navKey : Nav.Key
    , dashboard : WebData Dashboard
    , saveError : Maybe String
    }

type Msg
    = DashboardReceived (WebData Dashboard)
    | UpdateTitle String
    | UpdatePassword String
    | UpdateUsername String
    | UpdateHost String
    | UpdatePortNumber Int
    | UpdateDatabase String
    | UpdateDashboard
    | DashboardUpdated (Result Http.Error Dashboard)
    | Cancel

init : DashboardId -> Nav.Key -> ( Model, Cmd Msg )
init dashboardId navKey =
    ( initialModel navKey, getDashboard dashboardId )

initialModel : Nav.Key -> Model
initialModel navKey =
    { navKey = navKey
    , dashboard = RemoteData.Loading
    , saveError = Nothing
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DashboardReceived dashboard ->
            ( { model | dashboard = dashboard }, Cmd.none )
        
        UpdateTitle title ->
            let
                updateTitle = 
                    RemoteData.map
                        (\dashboard -> { dashboard | title = title })
                        model.dashboard
            in
            ( { model | dashboard = updateTitle }, Cmd.none )

        UpdatePassword password ->
            let
                dataSources = 
                    case model.dashboard of
                        RemoteData.Success dashboard -> dashboard.dataSource
                        _ -> { host = "", portNumber = 0, username = "", password = "", database = "" }
                updatePassword = 
                    RemoteData.map
                        (\dashboard -> { dashboard | dataSource = { dataSources | password = password } })
                        model.dashboard
            in
            ( { model | dashboard = updatePassword }, Cmd.none )
        
        UpdateUsername username ->
            let
                dataSources = 
                    case model.dashboard of
                        RemoteData.Success dashboard -> dashboard.dataSource
                        _ -> { host = "", portNumber = 0, username = "", password = "", database = "" }
                updateUsername = 
                    RemoteData.map
                        (\dashboard -> { dashboard | dataSource = { dataSources | username = username } })
                        model.dashboard
            in
            ( { model | dashboard = updateUsername }, Cmd.none )

        UpdateHost host ->
            let
                dataSources = 
                    case model.dashboard of
                        RemoteData.Success dashboard -> dashboard.dataSource
                        _ -> { host = "", portNumber = 0, username = "", password = "", database = "" }
                updateHost = 
                    RemoteData.map
                        (\dashboard -> { dashboard | dataSource = { dataSources | host = host } })
                        model.dashboard
            in
            ( { model | dashboard = updateHost }, Cmd.none )

        UpdatePortNumber portNumber ->
            let
                dataSources = 
                    case model.dashboard of
                        RemoteData.Success dashboard -> dashboard.dataSource
                        _ -> { host = "", portNumber = 0, username = "", password = "", database = "" }
                updatePortNumber = 
                    RemoteData.map
                        (\dashboard -> { dashboard | dataSource = { dataSources | portNumber = portNumber } })
                        model.dashboard
            in
            ( { model | dashboard = updatePortNumber }, Cmd.none )

        UpdateDatabase database ->
            let
                dataSources = 
                    case model.dashboard of
                        RemoteData.Success dashboard -> dashboard.dataSource
                        _ -> { host = "", portNumber = 0, username = "", password = "", database = "" }
                updateDatabase = 
                    RemoteData.map
                        (\dashboard -> { dashboard | dataSource = { dataSources | database = database } })
                        model.dashboard
            in
            ( { model | dashboard = updateDatabase }, Cmd.none )

        UpdateDashboard ->
            case model.dashboard of
                RemoteData.Success dashboard ->
                    ( model, updateDashboard dashboard )

                _ ->
                    ( model, Cmd.none )
            
        DashboardUpdated (Ok dashboard) ->
            let
                updatedDashboard = RemoteData.Success dashboard
            in
            ( { model | dashboard = updatedDashboard , saveError = Nothing }, 
                Route.pushUrl Route.Dashboards model.navKey
            )

        DashboardUpdated (Err httpError) ->
            ( { model | saveError = Just (buildErrorMessage httpError) }, Cmd.none )

        Cancel ->
            ( model, Route.pushUrl Route.Dashboards model.navKey )

view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Edit Dashboard" ]
        , viewDashboard model.dashboard
        , viewSaveError model.saveError
        ]


viewDashboard : WebData Dashboard -> Html Msg
viewDashboard dashboard =
    case dashboard of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading Dashboard..." ]

        RemoteData.Success dashboardData ->
            editForm dashboardData

        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)


editForm : Dashboard -> Html Msg
editForm dashboard =
    Html.form []
        [ div []
            [ text "Title"
            , br [] []
            , input
                [ type_ "text"
                , value dashboard.title
                , onInput UpdateTitle
                ]
                []
            ]
        , br [] []
        , div []
            [ text "Password"
            , br [] []
            , input
                [ type_ "password"
                , value dashboard.dataSource.password
                , onInput UpdatePassword
                ]
                []
            ]
        , br [] []
        , div []
            [ text "Username"
            , br [] []
            , input
                [ type_ "text"
                , value dashboard.dataSource.username
                , onInput UpdateUsername

                ]
                []
            ]
        , br [] []
        , div []
            [ text "Host"
            , br [] []
            , input
                [ type_ "text"
                , value dashboard.dataSource.host
                , onInput UpdateHost
                ]
                []
            ]
        , br [] []
        , div []
            [ text "Port Number"
            , br [] []
            , input
                [ type_ "number"
                , value (String.fromInt dashboard.dataSource.portNumber)
                , onInput (UpdatePortNumber << Maybe.withDefault 0 << String.toInt)
                ]
                []
            ]
        , br [] []
        , div []
            [ text "Database"
            , br [] []
            , input
                [ type_ "text"
                , value dashboard.dataSource.database
                , onInput UpdateDatabase
                ]
                []
            ]
        , br [] []
        , div []
            [ button [ type_ "button", onClick UpdateDashboard ]
                [ text "Submit" ]
            ,
             button [ type_ "button", onClick Cancel ]
                [ text "Cancel" ]
            ]
        ]


viewFetchError : String -> Html Msg
viewFetchError errorMessage =
    let
        errorHeading =
            "Couldn't fetch dashboard at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


viewSaveError : Maybe String -> Html msg
viewSaveError maybeError =
    case maybeError of
        Just error ->
            div []
                [ h3 [] [ text "Couldn't save dashboard at this time." ]
                , text ("Error: " ++ error)
                ]

        Nothing ->
            text ""

updateDashboard : Dashboard -> Cmd Msg
updateDashboard dashboard =
    Http.post
        { url = "http://127.0.0.1:6969/update-dashboard"
        , body = Http.jsonBody 
            (dashboardEncoder dashboard)
        , expect = Http.expectJson DashboardUpdated dashboardDecoder
        }

getDashboard : DashboardId -> Cmd Msg
getDashboard dashboardId =
    Http.post
        { url = "http://127.0.0.1:6969/get-dashboard"
        , body = Http.jsonBody 
            (Json.Encode.object [ ("dashboard_id", Dashboard.idEncoder dashboardId) ])
        , expect = Http.expectJson (DashboardReceived << RemoteData.fromResult) dashboardDecoder
        }
