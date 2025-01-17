module Page.DashboardPage exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Components.AddDashboardForm as EditDashboardForm exposing (emptyDashboard)
import Components.AddWidgetForm as AddWidgetForm
import Error exposing (buildErrorMessage)
import Html exposing (Html, button, div, h1, h3, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Json.Encode
import Config exposing (apiUrl)
import Models.Dashboard as Dashboard exposing (Dashboard, DashboardId, dashboardDecoder, dashboardEncoder)
import Models.Widgets exposing (Widget(..), widgetEncoder)
import RemoteData exposing (WebData)
import Widgets.Histogram
import Widgets.PieChart


type alias Model =
    { navKey : Nav.Key
    , dashboard : WebData Dashboard
    , addWidgetForm : AddWidgetForm.Model
    , editDashboardForm : EditDashboardForm.Model
    , showAddWidgetForm : Bool
    , showEditDashboardForm : Bool
    }


type Msg
    = DashboardReceived (WebData Dashboard)
    | ShowAddWidgetForm
    | ShowEditDashboardForm
    | WidgetFormChanged AddWidgetForm.Msg
    | DashboardFormChanged EditDashboardForm.Msg
    | DeleteWidget Int


init : DashboardId -> Nav.Key -> ( Model, Cmd Msg )
init dashboardId navKey =
    ( initialModel navKey, getDashboard dashboardId )


initialModel : Nav.Key -> Model
initialModel navKey =
    { navKey = navKey
    , dashboard = RemoteData.Loading
    , addWidgetForm = AddWidgetForm.init []
    , editDashboardForm = EditDashboardForm.init
    , showAddWidgetForm = False
    , showEditDashboardForm = False
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

        ShowAddWidgetForm ->
            ( { model | showAddWidgetForm = True }, Cmd.none )

        -- ( { model | showAddWidgetForm = True },
        ShowEditDashboardForm ->
            let
                currentDashboard =
                    case model.dashboard of
                        RemoteData.Success dashboardData ->
                            dashboardData

                        _ ->
                            emptyDashboard
            in
            ( { model | showEditDashboardForm = True, editDashboardForm = EditDashboardForm.Model currentDashboard }, Cmd.none )

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

        DashboardFormChanged formMsg ->
            case formMsg of
                EditDashboardForm.Cancel ->
                    ( { model | showEditDashboardForm = False }, Cmd.none )

                EditDashboardForm.AddNewDashboard ->
                    ( { model | showEditDashboardForm = False }, updateDashboard model.editDashboardForm.dashboard )

                _ ->
                    let
                        updatedForm =
                            EditDashboardForm.update formMsg model.editDashboardForm
                    in
                    ( { model | editDashboardForm = updatedForm }, Cmd.none )

        DeleteWidget widgetId ->
            case model.dashboard of
                RemoteData.Success dashboardData ->
                    ( model, deleteWidget dashboardData.dashboard_id widgetId )

                _ ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ case model.dashboard of
            RemoteData.Success dashboardData ->
                viewHeader dashboardData

            _ ->
                text ""
        , if model.showAddWidgetForm then
            Html.map WidgetFormChanged (AddWidgetForm.view model.addWidgetForm)

          else
            text ""
        , if model.showEditDashboardForm then
            Html.map DashboardFormChanged (EditDashboardForm.view model.editDashboardForm)

          else
            text ""
        , viewDashboard model.dashboard
        ]


viewHeader : Dashboard -> Html Msg
viewHeader dashboard =
    div [ class "widget-header" ]
        [ div [ class "header-center" ]
            [ h1 [] [ text dashboard.title ]
            , p [] [ text "Add widgets to your dashboard" ]
            ]
        , div [ class "header-left" ]
            [ viewAddWidgetButton
            , viewEditDashboardButton
            ]
        ]


viewAddWidgetButton : Html Msg
viewAddWidgetButton =
    div
        [ class "header-square add"
        , onClick ShowAddWidgetForm
        ]
        [ text "+" ]


viewEditDashboardButton : Html Msg
viewEditDashboardButton =
    div
        [ class "header-square"
        , onClick ShowEditDashboardForm
        ]
        [ text "Edit" ]


viewDashboard : WebData Dashboard -> Html Msg
viewDashboard dashboard =
    case dashboard of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading Dashboard..." ]

        RemoteData.Success dashboardData ->
            div [ class "widget-container" ]
                (List.map viewWidget dashboardData.widgets)

        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)


viewWidget : Widget -> Html Msg
viewWidget widget =
    case widget of
        Pie pieWidget ->
            div [ class "widget-square" ]
                [ Widgets.PieChart.view pieWidget.data
                , button [ class "delete-button", onClick (DeleteWidget pieWidget.widget_id) ] [ text "✖" ]
                , div [ class "widget-title" ] [ text pieWidget.title ]
                ]

        Histogram histogramWidget ->
            div [ class "widget-square" ]
                [ Widgets.Histogram.view histogramWidget.data
                , button [ class "delete-button", onClick (DeleteWidget histogramWidget.widget_id) ] [ text "✖" ]
                , div [ class "widget-title" ] [ text histogramWidget.title ]
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
        { url = apiUrl ++ "/get-dashboard"
        , body =
            Http.jsonBody
                (Json.Encode.object [ ( "dashboard_id", Dashboard.idEncoder dashboardId ) ])
        , expect = Http.expectJson (DashboardReceived << RemoteData.fromResult) dashboardDecoder
        }


addWidget : Widget -> DashboardId -> Cmd Msg
addWidget widget dashboardId =
    Http.post
        { url = apiUrl ++ "/add-widget"
        , body =
            Http.jsonBody
                (Json.Encode.object [ ( "dashboard_id", Dashboard.idEncoder dashboardId ), ( "widget", widgetEncoder widget ) ])
        , expect = Http.expectJson (DashboardReceived << RemoteData.fromResult) dashboardDecoder
        }


deleteWidget : DashboardId -> Int -> Cmd Msg
deleteWidget dashboardId widgetId =
    Http.post
        { url = apiUrl ++ "/delete-widget"
        , body =
            Http.jsonBody
                (Json.Encode.object [ ( "dashboard_id", Dashboard.idEncoder dashboardId ), ( "widget_id", Json.Encode.int widgetId ) ])
        , expect = Http.expectJson (DashboardReceived << RemoteData.fromResult) dashboardDecoder
        }


updateDashboard : Dashboard -> Cmd Msg
updateDashboard dashboard =
    Http.post
        { url = apiUrl ++ "/update-dashboard"
        , body =
            Http.jsonBody
                (dashboardEncoder dashboard)
        , expect = Http.expectJson (DashboardReceived << RemoteData.fromResult) dashboardDecoder
        }
