module Main exposing (main)

import Browser
import Url exposing (Url)
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (placeholder, type_, class)
import Html.Events exposing (..)
import Http

-- import Chart exposing (Chart)
-- import Chart.Pie exposing (slice)
-- import Svg.Attributes exposing (style)
-- import Chart exposing (Chart)
-- import Chart.Bar exposing (bars)

import Types exposing (..)
import Decoders exposing (..)
import Message exposing (..)
import Api exposing (..)

-- MAIN


main =
  Browser.application
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }



-- MODEL

type Loading a =
  Loading
  | Finished a

type alias Model =
    { dashboards : Loading (Result String (List Dashboard)) 
    , currentDashboard : Maybe Dashboard
    , newDashboard : Maybe Dashboard
    , showForm : Bool 
    , navKey : Navigation.Key
    , url : Url
    }

init : () -> Url -> Navigation.Key -> (Model, Cmd Msg)
init _ url key =
  ( { dashboards = Loading
    , currentDashboard = Nothing
    , newDashboard = Nothing
    , showForm = False
    , navKey = key
    , url = url
    }
  , get_dashboards
  )

-- UPDATE

errorToString : Http.Error -> String
errorToString error =
  case error of
    Http.BadUrl url ->
      "Bad URL: " ++ url

    Http.Timeout ->
      "Request timed out"

    Http.NetworkError ->
      "Network error"

    Http.BadStatus status ->
      "Bad status: " ++ String.fromInt status

    Http.BadBody body ->
      "Bad body: " ++ body


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotDashboards (Ok dashboard) ->
      ({ model | dashboards = Finished (Ok dashboard) }, Cmd.none)

    GotDashboards (Err error) ->
      let
          _ = Debug.log "HTTP Error" error
      in
      ({ model | dashboards = Finished (Err (errorToString error)) }, Cmd.none)

    UpdateCurrentDashboard (Ok dashboard) ->
      (updateCurrentDashboard model dashboard, Cmd.none)

    UpdateCurrentDashboard (Err _) ->
      (model, Cmd.none)

    CreateDashboard ->
      case model.newDashboard of
          Just dashboard ->
              (model, create_dashboard dashboard)
          Nothing ->
              (model, Cmd.none)

    NewPassword password ->
      case model.newDashboard of
          Just dashboard ->
              let
                  dataSource = dashboard.dataSource
                  newDataSource = { dataSource | password = password }
                  newDashboard = { dashboard | dataSource = newDataSource }
              in
              ({ model | newDashboard = Just newDashboard }, Cmd.none)
          Nothing ->
              let
                  newDataSource = { host = "", portNumber = 0, username = "", password = "", database = "" }
                  newDashboard = { dashboard_id = -1, title = "", widgets = [], dataSource = newDataSource }
              in
              ({ model | newDashboard = Just newDashboard }, Cmd.none)

    NewUsername username ->
      case model.newDashboard of
          Just dashboard ->
              let
                  dataSource = dashboard.dataSource
                  newDataSource = { dataSource | username = username }
                  newDashboard = { dashboard | dataSource = newDataSource }
              in
              ({ model | newDashboard = Just newDashboard }, Cmd.none)
          Nothing ->
              let
                  newDataSource = { host = "", portNumber = 0, username = "", password = "", database = "" }
                  newDashboard = { dashboard_id = -1, title = "", widgets = [], dataSource = newDataSource }
              in
              ({ model | newDashboard = Just newDashboard }, Cmd.none)

    NewHost host ->
      case model.newDashboard of
          Just dashboard ->
              let
                  dataSource = dashboard.dataSource
                  newDataSource = { dataSource | host = host }
                  newDashboard = { dashboard | dataSource = newDataSource }
              in
              ({ model | newDashboard = Just newDashboard }, Cmd.none)
          Nothing ->
              let
                  newDataSource = { host = "", portNumber = 0, username = "", password = "", database = "" }
                  newDashboard = { dashboard_id = -1, title = "", widgets = [], dataSource = newDataSource }
              in
              ({ model | newDashboard = Just newDashboard }, Cmd.none)

    NewPort hostPort ->
      case model.newDashboard of
          Just dashboard ->
              let
                  dataSource = dashboard.dataSource
                  newDataSource = { dataSource | portNumber = String.toInt hostPort |> Maybe.withDefault 0 }
                  newDashboard = { dashboard | dataSource = newDataSource }
              in
              ({ model | newDashboard = Just newDashboard }, Cmd.none)
          Nothing ->
              let
                  newDataSource = { host = "", portNumber = 0, username = "", password = "", database = "" }
                  newDashboard = { dashboard_id = -1, title = "", widgets = [], dataSource = newDataSource }
              in
              ({ model | newDashboard = Just newDashboard }, Cmd.none)

    NewDatabase database ->
      case model.newDashboard of
          Just dashboard ->
              let
                  dataSource = dashboard.dataSource
                  newDataSource = { dataSource | database = database }
                  newDashboard = { dashboard | dataSource = newDataSource }
              in
              ({ model | newDashboard = Just newDashboard }, Cmd.none)
          Nothing ->
              let
                  newDataSource = { host = "", portNumber = 0, username = "", password = "", database = "" }
                  newDashboard = { dashboard_id = -1, title = "", widgets = [], dataSource = newDataSource }
              in
              ({ model | newDashboard = Just newDashboard }, Cmd.none)

    NewTitle title ->
      case model.newDashboard of
          Just dashboard ->
              let
                  newDashboard = { dashboard | title = title }
              in
              ({ model | newDashboard = Just newDashboard }, Cmd.none)
          Nothing ->
              let
                  newDataSource = { host = "", portNumber = 0, username = "", password = "", database = "" }
                  newDashboard = { dashboard_id = -1, title = "", widgets = [], dataSource = newDataSource }
              in
              ({ model | newDashboard = Just newDashboard }, Cmd.none)

    ShowForm ->
      ({ model | showForm = True }, Cmd.none)

    CloseForm ->
      ({ model | showForm = False , newDashboard = Nothing }, Cmd.none)

    NavigateToDashboard dashboardResult ->
      case dashboardResult of
        Ok newDashboard ->
          let
              newUrl = "/dashboard/" ++ String.fromInt newDashboard.dashboard_id
          in
          ({ model | currentDashboard = Just newDashboard , showForm = False , newDashboard = Nothing }, Navigation.pushUrl model.navKey newUrl)
          
        Err error ->
          let
              _ = Debug.log "HTTP Error" error
          in
          (model, Cmd.none)
    
    NavigateToHome ->
      ({ model | currentDashboard = Nothing }, Navigation.pushUrl model.navKey "/")

    UrlChanged url ->
      case model.currentDashboard of
        Just _ ->
          let
              _ = Debug.log "URL Changed" url
          in
          (model, Cmd.none)

        Nothing ->
          let
              _ = Debug.log "URL Changed" url
          in
          (model, get_dashboards)

    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          (model, Navigation.pushUrl model.navKey (Url.toString url))

        Browser.External href ->
          (model, Navigation.load href)


updateCurrentDashboard : Model -> Dashboard -> Model
updateCurrentDashboard model dashboard =
  { model | currentDashboard = Just dashboard }

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW

renderDashboardSquare : Dashboard -> Html Msg
renderDashboardSquare dashboard =
    div 
        [ class "dashboard-square"
        , onClick (NavigateToDashboard (Ok dashboard))
        ]
        [ text dashboard.title ]

renderAddDashboardSquare : Html Msg
renderAddDashboardSquare =
    div 
        [ class "dashboard-square"
        , onClick ShowForm
        ]
        [ text "+" ]


renderAddNewDashboardForm : Html Msg
renderAddNewDashboardForm =
  div [ class "form" ]
          [ div [ class "form-content" ]
            [
              div [ class "form-group" ]
                  [ label [] [ text "Password" ]
                  , input
                      [ type_ "password"
                      , placeholder "Enter your password"
                      , onInput NewPassword
                      ]
                      []
                  ]
              , div [ class "form-group" ]
                  [ label [] [ text "Username" ]
                  , input
                      [ type_ "text"
                      , placeholder "Enter your username"
                      , onInput NewUsername
                      ]
                      []
                  ]
              , div [ class "form-group" ]
                  [ label [] [ text "Host" ]
                  , input
                      [ type_ "text"
                      , placeholder "Enter your host"
                      , onInput NewHost
                      ]
                      []
                  ]
              , div [ class "form-group" ]
                  [ label [] [ text "Port" ]
                  , input
                      [ type_ "text"
                      , placeholder "Enter your port"
                      , onInput NewPort
                      ]
                      []
                  ]
              , div [ class "form-group" ]
                  [ label [] [ text "Database" ]
                  , input
                      [ type_ "text"
                      , placeholder "Enter your database"
                      , onInput NewDatabase
                      ]
                      []
                  ]
              , div [ class "form-group" ]
                  [ label [] [ text "Dashboard Title" ]
                  , input
                      [ type_ "text"
                      , placeholder "Enter dashboard title"
                      , onInput NewTitle
                      ]
                      []
                  ]
              , div [ class "form-buttons" ]
                  [ button [ onClick CloseForm ] [ text "Cancel" ]
                  , button [ onClick CreateDashboard ] [ text "Create" ]
                  ]
              ]
          ]
          

view : Model -> Browser.Document Msg
view model =
  { title = 
      case model.currentDashboard of
        Just dashboard ->
          "Dashboard " ++ String.fromInt dashboard.dashboard_id

        Nothing ->
          "Dashboard"
  , body =
      node "link" 
          [ Html.Attributes.rel "stylesheet"
          , Html.Attributes.href "/styles/main.css"
          ]
          []
      ::
      case model.currentDashboard of
        Just dashboard ->
          [div [] [ div [] 
            [ text "Dashboard " 
            , text (String.fromInt dashboard.dashboard_id) 
            , button [ onClick (NavigateToHome) ] [ text "Back"]
            ]
          ]
          ]

        Nothing ->
          case model.dashboards of
            Loading ->
              [ div [] [ text "Loading..." ] ]

            Finished (Ok dashboards) ->
              [ div [] (List.map renderDashboardSquare dashboards) 
              , renderAddDashboardSquare
              , if model.showForm then renderAddNewDashboardForm else div [] []
              ]
            
            Finished (Err error) ->
              [ div [] [ text error ] ]
  }
