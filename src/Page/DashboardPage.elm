module Page.DashboardPage exposing (..)

import Browser.Navigation as Nav
import Models.Dashboard as Dashboard exposing (..)
import Models.DataSource exposing (..)
import Models.Widgets exposing (..)
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (class, href, rel, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Json.Encode
import List.Extra
import RemoteData exposing (WebData)
import Widgets.Histogram
import Widgets.PieChart

type alias Model =
    { navKey : Nav.Key
    , dashboard : WebData Dashboard
    , showAddWidgetForm : Bool
    , selectedTable : Table
    , isWidgetTypeSelected : Bool
    , newWidget : Widget
    }


type Msg
    = DashboardReceived (WebData Dashboard)
    | ShowForm
    | FormChanged FormMsg
    | DeleteWidget Int


type FormMsg
    = TableSelected String
    | WidgetTypeSelected String
    | ColumnToggled String
    | Cancel
    | AddNewWidget


init : DashboardId -> Nav.Key -> ( Model, Cmd Msg )
init dashboardId navKey =
    ( initialModel navKey, getDashboard dashboardId )


initialModel : Nav.Key -> Model
initialModel navKey =
    { navKey = navKey
    , dashboard = RemoteData.Loading
    , showAddWidgetForm = False
    , selectedTable = Table "" []
    , isWidgetTypeSelected = False
    , newWidget = emptyWidget "PieChart"
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DashboardReceived response ->
            let
                clearedModel =
                    initialModel model.navKey
            in
            ( { clearedModel | dashboard = response }, Cmd.none )

        ShowForm ->
            ( { model | showAddWidgetForm = True }, Cmd.none )

        FormChanged formMsg ->
            updateForm formMsg model

        DeleteWidget widgetId ->
            case model.dashboard of
                RemoteData.Success dashboardData ->
                    ( model, deleteWidget dashboardData.dashboard_id widgetId )

                _ ->
                    ( model, Cmd.none )

updateForm : FormMsg -> Model -> ( Model, Cmd Msg )
updateForm formMsg model =
    case formMsg of
        Cancel ->
            let
                clearedModel = clearWidgetForm model
            in
            ( { clearedModel | showAddWidgetForm = False }, Cmd.none )

        TableSelected tableName ->
            updateTableSelection tableName model

        ColumnToggled columnName ->
            toggleColumn columnName model

        WidgetTypeSelected widgetType ->
            ( { model | newWidget = emptyWidget widgetType, isWidgetTypeSelected = True }, Cmd.none )

        AddNewWidget ->
            case model.dashboard of
                RemoteData.Success dashboardData ->
                    let
                        clearedModel =
                            clearWidgetForm model
                    in
                    ( { clearedModel | showAddWidgetForm = False }
                    , addWidget model.newWidget dashboardData.dashboard_id
                    )

                _ ->
                    ( model, Cmd.none )


clearWidgetForm : Model -> Model
clearWidgetForm model =
    { model | newWidget = emptyWidget "" , isWidgetTypeSelected = False, selectedTable = Table "" [] }

emptyWidget : String -> Widget
emptyWidget widgetType =
    case widgetType of
        "PieChart" ->
            Pie { widget_id = 0, title = "", table = "", data_column = "", data = [] }

        "Histogram" ->
            Histogram { widget_id = 0, title = "", table = "", data_column = "", data = [] }

        _ ->
            Pie { widget_id = 0, title = "", table = "", data_column = "", data = [] }


updateTableSelection : String -> Model -> ( Model, Cmd Msg )
updateTableSelection tableName model =
    let
        selectedTable =
            case model.dashboard of
                RemoteData.Success dashboardData ->
                    List.Extra.find (\table -> table.name == tableName) dashboardData.dataSource.tables

                _ ->
                    Nothing

        updatedWidget =
            case model.newWidget of
                Pie data ->
                    Pie { data | table = tableName }

                Histogram data ->
                    Histogram { data | table = tableName }
    in
    case selectedTable of
        Just table ->
            ( { model | selectedTable = table, newWidget = updatedWidget }, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


toggleColumn : String -> Model -> ( Model, Cmd Msg )
toggleColumn columnName model =
    let
        updatedWidget =
            case model.newWidget of
                Pie data ->
                    Pie { data | data_column = columnName }

                Histogram data ->
                    Histogram { data | data_column = columnName }

    in
    ( { model | newWidget = updatedWidget }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Widgets" ]
        , viewAddWidgetButton
        , if model.showAddWidgetForm then
            viewAddWidgetForm model

          else
            text ""
        , viewDashboard model.dashboard
        , node "link"
            [ rel "stylesheet"
            , href "/styles/main.css"
            ]
            []
        ]


viewAddWidgetForm : Model -> Html Msg
viewAddWidgetForm model =
    case model.dashboard of
        RemoteData.Success dashboardData ->
            div [ class "add-widget form" ]
                [ h3 [] [ text "Add Widget" ]
                , div []
                    [ select
                        [ class "table-dropdown", onInput (FormChanged << WidgetTypeSelected) ]
                        [ option [ value "PieChart" ] [ text "Pie Chart" ]
                        , option [ value "Histogram" ] [ text "Histogram" ]
                        ]
                    ]
                , if model.isWidgetTypeSelected then
                    div []
                        [ select
                            [ class "table-dropdown", onInput (FormChanged << TableSelected) ]
                            (List.map (\table -> option [ value table.name ] [ text table.name ]) dashboardData.dataSource.tables)
                        , div [] [ text "Select columns:" ] 
                        , viewTableColumns model.selectedTable
                        ]
                  else
                    text ""
                , button [ class "add-widget-button", onClick (FormChanged AddNewWidget) ] [ text "Add Widget" ]
                , button [ class "cancel-button", onClick (FormChanged Cancel) ] [ text "Cancel" ]
                ]
            
        _ ->
            text ""


viewTableColumns : Table -> Html Msg
viewTableColumns table =
    div []
        [
            select [ class "table-dropdown", onInput (FormChanged << ColumnToggled) ]
                (List.map (\col -> option [ value col.name ] [ text col.name ]) table.columns)
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