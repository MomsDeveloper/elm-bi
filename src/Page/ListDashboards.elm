module Page.ListDashboards exposing (Model, Msg, init, update, view)

import Dashboard exposing (..)
import RemoteData exposing (WebData)
import Http exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Browser.Navigation as Nav
import Html.Attributes exposing (href, type_, class, rel)
import Error exposing (buildErrorMessage)
import Json.Encode 
import Route


type alias Model =
    { dashboards : WebData (List Dashboard)
    , newDashboard : Dashboard
    , showAddDashboardForm : Bool
    , deleteError : Maybe String
    , createError : Maybe String
    , navKey : Nav.Key
    }

type Msg
    = FetchDashboards
    | DashboardsReceived (WebData (List Dashboard))
    | DeleteDashboard DashboardId
    | DashboardDeleted (Result Http.Error (List Dashboard))
    | UpdateTitle String
    | UpdatePassword String
    | UpdateUsername String
    | UpdateHost String
    | UpdatePortNumber Int
    | UpdateDatabase String
    | ShowForm
    | Cancel
    | AddNewDashboard
    | DashboardCreated (Result Http.Error Dashboard)
    | GoToDashboard DashboardId

init : Nav.Key -> ( Model, Cmd Msg )
init navKey =
    ( initialModel navKey, fetchDashboards )

initialModel : Nav.Key -> Model
initialModel navKey =
    { dashboards = RemoteData.NotAsked
    , newDashboard = emptyDashboard
    , showAddDashboardForm = False
    , deleteError = Nothing
    , createError = Nothing
    , navKey = navKey
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchDashboards ->
            ( { model | dashboards = RemoteData.Loading }, fetchDashboards )

        DashboardsReceived response ->
            ( { model | dashboards = response }, Cmd.none )

        DeleteDashboard dashboardId ->
            ( model, deleteDashboard dashboardId )

        DashboardDeleted (Ok _ ) ->
            ( model, fetchDashboards )

        DashboardDeleted (Err httpError) ->
            ( { model | deleteError = Just (buildErrorMessage httpError) }, Cmd.none )

        UpdatePassword password ->
            let
                dataSource = model.newDashboard.dataSource
                oldDashboard = model.newDashboard
                updatedDataSource = { dataSource | password = password }
                updatedDashboard = { oldDashboard | dataSource = updatedDataSource }
            in
            ( { model | newDashboard = updatedDashboard }, Cmd.none )
        
        UpdateUsername username ->
            let
                dataSource = model.newDashboard.dataSource
                oldDashboard = model.newDashboard
                updatedDataSource = { dataSource | username = username }
                updatedDashboard = { oldDashboard | dataSource = updatedDataSource }
            in
            ( { model | newDashboard = updatedDashboard }, Cmd.none )

        UpdateHost host ->
            let
                dataSource = model.newDashboard.dataSource
                oldDashboard = model.newDashboard
                updatedDataSource = { dataSource | host = host }
                updatedDashboard = { oldDashboard | dataSource = updatedDataSource }
            in
            ( { model | newDashboard = updatedDashboard }, Cmd.none )

        UpdatePortNumber portNumber ->
            let
                dataSource = model.newDashboard.dataSource
                oldDashboard = model.newDashboard
                updatedDataSource = { dataSource | portNumber = portNumber }
                updatedDashboard = { oldDashboard | dataSource = updatedDataSource }
            in
            ( { model | newDashboard = updatedDashboard }, Cmd.none )

        UpdateDatabase database ->
            let
                dataSource = model.newDashboard.dataSource
                oldDashboard = model.newDashboard
                updatedDataSource = { dataSource | database = database }
                updatedDashboard = { oldDashboard | dataSource = updatedDataSource }
            in
            ( { model | newDashboard = updatedDashboard }, Cmd.none )

        UpdateTitle title ->
            let
                oldDashboard = model.newDashboard
                updatedDashboard = { oldDashboard | title = title }
            in
            ( { model | newDashboard = updatedDashboard }, Cmd.none )
        
        ShowForm ->
            ( { model | showAddDashboardForm = True }, Cmd.none )
    
        Cancel ->
            ( { model | showAddDashboardForm = False }, Cmd.none )

        AddNewDashboard ->
            ( model, create_dashboard model.newDashboard )
        
        DashboardCreated (Ok dashboard) ->
            ( model, Route.pushUrl (Route.Dashboard dashboard.dashboard_id) model.navKey )
        
        DashboardCreated (Err httpError) ->
            ( { model | createError = Just (buildErrorMessage httpError) }, Cmd.none )

        GoToDashboard dashboardId ->
            ( model, Route.pushUrl (Route.Dashboard dashboardId) model.navKey )
            

emptyDashboard : Dashboard
emptyDashboard =
    { dashboard_id = (DashboardId -1)
    , title = ""
    , dataSource = { host = "", portNumber = 0, username = "", password = "", database = "" }
    , widgets = []
    }

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick FetchDashboards ]
            [ text "Refresh dashboards" ]
        , viewDashboards model.dashboards
        , viewAddDashboardButton
        , if model.showAddDashboardForm then
            viewAddDashboardForm
          else
            text ""
        , viewDeleteError model.deleteError
        , viewAddError model.createError
        , node "link" 
            [ rel "stylesheet"
            , href "/styles/main.css"] []
        ]


viewAddDashboardForm : Html Msg
viewAddDashboardForm  =
    div [ class "form" ]
          [ div [ class "form-content" ]
            [
              div [ class "form-group" ]
                  [ label [] [ text "Password" ]
                  , input
                      [ type_ "password"
                        , onInput UpdatePassword
                        ]
                      []
                  ]
              , div [ class "form-group" ]
                  [ label [] [ text "Username" ]
                  , input
                      [ type_ "text"
                        , onInput UpdateUsername
                        ]
                      []
                  ]
              , div [ class "form-group" ]
                  [ label [] [ text "Host" ]
                  , input
                      [ type_ "text"
                        , onInput UpdateHost
                        ]
                      []
                  ]
              , div [ class "form-group" ]
                  [ label [] [ text "Port" ]
                  , input
                      [ type_ "number"
                        , onInput (UpdatePortNumber << Maybe.withDefault 0 << String.toInt)
                        ]
                      []
                  ]
              , div [ class "form-group" ]
                  [ label [] [ text "Database" ]
                  , input
                      [ type_ "text"
                        , onInput UpdateDatabase
                        ]
                      []
                  ]
              , div [ class "form-group" ]
                  [ label [] [ text "Dashboard Title" ]
                  , input
                      [ type_ "text"
                        , onInput UpdateTitle
                        ]
                      []
                  ]
              , div [ class "form-buttons" ]
                  [ button [ onClick Cancel ] [ text "Cancel" ]
                  , button [ onClick AddNewDashboard ] [ text "Create" ]
                  ]
              ]
          ]

viewAddDashboardButton :  Html Msg
viewAddDashboardButton =
    div 
        [ class "dashboard-square"
        , onClick ShowForm
        ]
        [ text "+" ]

viewDashboards : WebData (List Dashboard) -> Html Msg
viewDashboards dashboards =
    case dashboards of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success actualDashboards ->
            div []
                [ h3 [] [ text "Dashboards" ]
                , table []
                    ([ ] ++ List.map viewDashboard actualDashboards)
                ]

        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)


viewDashboard : Dashboard -> Html Msg
viewDashboard dashboard =
    tr []
        [ 
        td [ class "dashboard-square", onClick (GoToDashboard dashboard.dashboard_id) ]
            [ text dashboard.title ]
        , td []
            [ button [ type_ "button", onClick (DeleteDashboard dashboard.dashboard_id) ] [ text "Delete" ] ]
        ]


viewFetchError : String -> Html Msg
viewFetchError errorMessage =
    let
        errorHeading =
            "Couldn't fetch dashboards at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]

viewDeleteError : Maybe String -> Html msg
viewDeleteError maybeError =
    case maybeError of
        Just error ->
            div []
                [ h3 [] [ text "Couldn't delete post at this time." ]
                , text ("Error: " ++ error)
                ]

        Nothing ->
            text ""

viewAddError : Maybe String -> Html msg
viewAddError maybeError =
    case maybeError of
        Just error ->
            div []
                [ h3 [] [ text "Couldn't add dashboard at this time." ]
                , text ("Error: " ++ error)
                ]

        Nothing ->
            text ""

fetchDashboards : Cmd Msg
fetchDashboards =
  Http.post
    { url = "http://127.0.0.1:6969/get-dashboards"
    , body = Http.emptyBody
    , expect = 
        dashboardsDecoder
          |>
    Http.expectJson (RemoteData.fromResult >> DashboardsReceived)
    }

deleteDashboard : DashboardId -> Cmd Msg
deleteDashboard dashboardId =
    Http.post
        { url = "http://127.0.0.1:6969/delete-dashboard"
        , body = Http.jsonBody 
            (Json.Encode.object [ ("dashboard_id", Dashboard.idEncoder dashboardId) ])
        , expect = Http.expectJson DashboardDeleted dashboardsDecoder
        }

create_dashboard : Dashboard -> Cmd Msg
create_dashboard dashboard =
  Http.post
    { url = "http://127.0.0.1:6969/create-dashboard"
    , body = Http.jsonBody 
        (dashboardEncoder dashboard )
        
    , expect = Http.expectJson DashboardCreated dashboardDecoder
    }
