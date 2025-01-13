module Page.DashboardPage exposing (..)

import Browser.Navigation as Nav
import Components.AddWidgetForm as AddWidgetForm exposing (..)
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (class, href, rel)
import Html.Events exposing (onClick)
import Http exposing (..)
import Json.Encode
import Models.Dashboard as Dashboard exposing (..)
import Models.DataSource exposing (..)
import Models.Widgets exposing (..)
import RemoteData exposing (WebData)
import Widgets.Histogram
import Widgets.PieChart


type alias Model =
    { navKey : Nav.Key
    , dashboard : WebData Dashboard
    , addWidgetForm : AddWidgetForm.Model
    , showAddWidgetForm : Bool
    }


type Msg
    = DashboardReceived (WebData Dashboard)
    | ShowForm
    | WidgetFormChanged AddWidgetForm.Msg
    | DeleteWidget Int


init : DashboardId -> Nav.Key -> ( Model, Cmd Msg )
init dashboardId navKey =
    ( initialModel navKey, getDashboard dashboardId )


initialModel : Nav.Key -> Model
initialModel navKey =
    { navKey = navKey
    , dashboard = RemoteData.Loading
    , addWidgetForm = AddWidgetForm.init []
    , showAddWidgetForm = False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DashboardReceived response ->
            let
                clearedModel =
                    initialModel model.navKey

                updatedModel =
                    case response of
                        RemoteData.Success dashboardData ->
                            { clearedModel
                                | dashboard = response
                                , addWidgetForm = AddWidgetForm.init dashboardData.dataSource.tables
                            }

                        _ ->
                            { clearedModel | dashboard = response }
            in
            ( updatedModel, Cmd.none )

        ShowForm ->
            ( { model | showAddWidgetForm = True }, Cmd.none )

        WidgetFormChanged formMsg ->
            case formMsg of
                AddWidgetForm.Cancel ->
                    let
                        updatedForm =
                            AddWidgetForm.update formMsg model.addWidgetForm
                    in
                    ( { model | showAddWidgetForm = False, addWidgetForm = updatedForm }, Cmd.none )

                AddWidgetForm.AddNewWidget ->
                    case model.dashboard of
                        RemoteData.Success dashboardData ->
                            let
                                widget =
                                    model.addWidgetForm.widget

                                dashboardId =
                                    dashboardData.dashboard_id
                            in
                            ( { model | showAddWidgetForm = False }, addWidget widget dashboardId )

                        _ ->
                            ( { model | showAddWidgetForm = False }, Cmd.none )

                _ ->
                    let
                        updatedForm =
                            AddWidgetForm.update formMsg model.addWidgetForm
                    in
                    ( { model | addWidgetForm = updatedForm }, Cmd.none )

        DeleteWidget widgetId ->
            case model.dashboard of
                RemoteData.Success dashboardData ->
                    ( model, deleteWidget dashboardData.dashboard_id widgetId )

                _ ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Widgets" ]
        , viewAddWidgetButton
        , if model.showAddWidgetForm then
            Html.map WidgetFormChanged (AddWidgetForm.view model.addWidgetForm)

          else
            text ""
        , viewDashboard model.dashboard
        , node "link"
            [ rel "stylesheet"
            , href "/styles/main.css"
            ]
            []
        ]


viewAddWidgetButton : Html Msg
viewAddWidgetButton =
    div
        [ class "widget-square"
        , onClick ShowForm
        ]
        [ text "+" ]


viewDashboard : WebData Dashboard -> Html Msg
viewDashboard dashboard =
    case dashboard of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading Dashboard..." ]

        RemoteData.Success dashboardData ->
            div []
                [ h3 [] [ text dashboardData.title ]
                , div []
                    (List.map viewWidget dashboardData.widgets)
                ]

        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)


viewWidget : Widget -> Html Msg
viewWidget widget =
    case widget of
        Pie pieWidget ->
            div [ class "widget-square" ]
                [ Widgets.PieChart.view pieWidget.data
                , button [ class "delete-widget-button", onClick (DeleteWidget pieWidget.widget_id) ] [ text "Delete" ]
                ]

        Histogram histogramWidget ->
            div [ class "widget-square" ]
                [ Widgets.Histogram.view histogramWidget.data
                , button [ class "delete-widget-button", onClick (DeleteWidget histogramWidget.widget_id) ] [ text "Delete" ]
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


getDashboard : DashboardId -> Cmd Msg
getDashboard dashboardId =
    Http.post
        { url = "http://127.0.0.1:6969/get-dashboard"
        , body =
            Http.jsonBody
                (Json.Encode.object [ ( "dashboard_id", Dashboard.idEncoder dashboardId ) ])
        , expect = Http.expectJson (DashboardReceived << RemoteData.fromResult) dashboardDecoder
        }


addWidget : Widget -> DashboardId -> Cmd Msg
addWidget widget dashboardId =
    Http.post
        { url = "http://127.0.0.1:6969/add-widget"
        , body =
            Http.jsonBody
                (Json.Encode.object [ ( "dashboard_id", Dashboard.idEncoder dashboardId ), ( "widget", widgetEncoder widget ) ])
        , expect = Http.expectJson (DashboardReceived << RemoteData.fromResult) dashboardDecoder
        }


deleteWidget : DashboardId -> Int -> Cmd Msg
deleteWidget dashboardId widgetId =
    Http.post
        { url = "http://127.0.0.1:6969/delete-widget"
        , body =
            Http.jsonBody
                (Json.Encode.object [ ( "dashboard_id", Dashboard.idEncoder dashboardId ), ( "widget_id", Json.Encode.int widgetId ) ])
        , expect = Http.expectJson (DashboardReceived << RemoteData.fromResult) dashboardDecoder
        }



-- updateDashboard : DashboardInfo -> Cmd Msg
-- updateDashboard dashboardInfo =
--     Http.post
--         { url = "http://127.0.0.1:6969/update-dashboard"
--         , body =
--             Http.jsonBody
--                 (dashboardInfoEncoder dashboardInfo)
--         , expect = Http.expectJson (DashboardReceived << RemoteData.fromResult) dashboardDecoder
--         }
