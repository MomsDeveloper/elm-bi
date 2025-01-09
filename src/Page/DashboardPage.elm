module Page.DashboardPage exposing (..)

import Browser.Navigation as Nav
import Models.Dashboard as Dashboard exposing (..)
import Models.DataSource exposing (..)
import Models.Widgets exposing (..)
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (class, disabled, href, rel, value)
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
    , widgetsData : List (WebData WidgetData)
    , tables : WebData (List Table)
    , showAddWidgetForm : Bool
    , selectedTable : Table
    , formButtonsDisabled : Bool
    , newWidget : Widget
    }


type Msg
    = DashboardReceived (WebData Dashboard)
    | PieDataReceived (WebData (List PieData))
    | HistogramDataReceived (WebData (List Float))
    | TablesReceived (WebData (List Table))
    | ShowForm
    | FormChanged FormMsg


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
    , widgetsData = []
    , showAddWidgetForm = False
    , tables = RemoteData.NotAsked
    , selectedTable = Table "" []
    , formButtonsDisabled = False
    , newWidget = emptyWidget "PieChart"
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DashboardReceived response ->
            let
                cmd =
                    case response of
                        RemoteData.Success dashboard ->
                            getDataSourceTables dashboard.dashboard_id

                        _ ->
                            Cmd.none
            in
            ( { model | dashboard = response, widgetsData = [], tables = RemoteData.NotAsked }, cmd )

        PieDataReceived response ->
            updateWidgetData model response Piedata

        HistogramDataReceived response ->
            updateWidgetData model response Histogramdata

        TablesReceived response ->
            let
                newModel =
                    case response of
                        RemoteData.Success (head :: _) ->
                            { model | tables = response, selectedTable = head }

                        _ ->
                            { model | tables = response }
            in
            ( newModel, fetchWidgetsData model )

        ShowForm ->
            ( { model | showAddWidgetForm = True }, Cmd.none )

        FormChanged formMsg ->
            updateForm formMsg model


updateForm : FormMsg -> Model -> ( Model, Cmd Msg )
updateForm formMsg model =
    case formMsg of
        Cancel ->
            ( { model | showAddWidgetForm = False }, Cmd.none )

        TableSelected tableName ->
            updateTableSelection tableName model

        ColumnToggled columnName ->
            toggleColumn columnName model

        WidgetTypeSelected widgetType ->
            ( { model | newWidget = emptyWidget widgetType }, Cmd.none )

        AddNewWidget ->
            case model.dashboard of
                RemoteData.Success dashboardData ->
                    ( { model | showAddWidgetForm = False, newWidget = emptyWidget "", formButtonsDisabled = False }
                    , addWidget model.newWidget dashboardData.dashboard_id
                    )

                _ ->
                    ( model, Cmd.none )


emptyWidget : String -> Widget
emptyWidget widgetType =
    case widgetType of
        "PieChart" ->
            Pie { widget_id = 0, title = "", table = "", data_column = "" }

        "Histogram" ->
            Histogram { widget_id = 0, title = "", table = "", data_column = "" }

        _ ->
            Pie { widget_id = 0, title = "", table = "", data_column = "" }


updateWidgetData : Model -> WebData a -> (a -> WidgetData) -> ( Model, Cmd Msg )
updateWidgetData model response constructor =
    ( { model | widgetsData = model.widgetsData ++ [ RemoteData.map constructor response ] }, Cmd.none )


updateTableSelection : String -> Model -> ( Model, Cmd Msg )
updateTableSelection tableName model =
    let
        selectedTable =
            List.Extra.find (\table -> table.name == tableName) (RemoteData.withDefault [] model.tables)

        updatedWidget =
            case model.newWidget of
                Pie data ->
                    Pie { data | table = tableName }

                Histogram data ->
                    Histogram { data | table = tableName }
    in
    case selectedTable of
        Just table ->
            ( { model | selectedTable = table, formButtonsDisabled = False, newWidget = updatedWidget }, Cmd.none )

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

        isFormDisabled =
            not model.formButtonsDisabled
    in
    ( { model | formButtonsDisabled = isFormDisabled, newWidget = updatedWidget }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Widgets" ]
        , viewAddWidgetButton
        , if model.showAddWidgetForm then
            viewAddWidgetForm model

          else
            text ""
        , viewDashboard model.dashboard model.widgetsData
        , node "link"
            [ rel "stylesheet"
            , href "/styles/main.css"
            ]
            []
        ]


viewAddWidgetForm : Model -> Html Msg
viewAddWidgetForm model =
    case model.tables of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            div [] [ text "Loading tables..." ]

        RemoteData.Success tableList ->
            div [ class "add-widget-form" ]
                [ h3 [] [ text "Add Widget" ]
                , div []
                    [ select
                        [ class "table-dropdown", onInput (FormChanged << WidgetTypeSelected) ]
                        [ option [ value "PieChart" ] [ text "Pie Chart" ]
                        , option [ value "Histogram" ] [ text "Histogram" ]
                        ]
                    ]
                , select
                    [ class "table-dropdown", onInput (FormChanged << TableSelected) ]
                    (List.map (\table -> option [ value table.name ] [ text table.name ]) tableList)
                , div [] [ text "Select columns:" ]
                , viewTableColumns model
                , button [ class "add-widget-button", onClick (FormChanged AddNewWidget) ] [ text "Add Widget" ]
                , button [ class "cancel-button", onClick (FormChanged Cancel) ] [ text "Cancel" ]
                ]

        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)


viewTableColumns : Model -> Html Msg
viewTableColumns model =
    div []
        (List.map
            (\col ->
                div []
                    [ button
                        [ onClick ((FormChanged << ColumnToggled) col.name)

                        -- , disabled (model.newWidget.data_column /= col.name && model.formButtonsDisabled)
                        ]
                        [ text col.name ]
                    ]
            )
            model.selectedTable.columns
        )


viewAddWidgetButton : Html Msg
viewAddWidgetButton =
    div
        [ class "widget-square"
        , onClick ShowForm
        ]
        [ text "+" ]


viewDashboard : WebData Dashboard -> List (WebData WidgetData) -> Html Msg
viewDashboard dashboard widgetsData =
    case dashboard of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading Dashboard..." ]

        RemoteData.Success dashboardData ->
            div []
                [ h3 [] [ text dashboardData.title ]
                , div []
                    ([] ++ List.map viewWidget widgetsData)
                ]

        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)


viewWidget : WebData WidgetData -> Html Msg
viewWidget widgetData =
    case widgetData of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading Widget..." ]

        RemoteData.Success data ->
            case data of
                Piedata pieData ->
                    div [ class "widget-square" ] [ Widgets.PieChart.view pieData ]

                Histogramdata histogramData ->
                    div [ class "widget-square" ] [ Widgets.Histogram.view histogramData ]

        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)


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


fetchWidgetsData : Model -> Cmd Msg
fetchWidgetsData model =
    case model.dashboard of
        RemoteData.Success dashboardData ->
            Cmd.batch (List.map (\widget -> fetchWidgetData dashboardData.dashboard_id widget) dashboardData.widgets)

        _ ->
            Cmd.none


fetchWidgetData : DashboardId -> Widget -> Cmd Msg
fetchWidgetData dashboardId widget =
    case widget of
        Pie pie ->
            fetchPieData dashboardId pie.widget_id

        Histogram histogram ->
            fetchHistogramData dashboardId histogram.widget_id


fetchPieData : DashboardId -> Int -> Cmd Msg
fetchPieData dashboardId widgetId =
    Http.post
        { url = "http://127.0.0.1:6969/fetch-widget-data"
        , body =
            Http.jsonBody
                (Json.Encode.object [ ( "dashboard_id", Dashboard.idEncoder dashboardId ), ( "widget_id", Json.Encode.int widgetId ) ])
        , expect = Http.expectJson (PieDataReceived << RemoteData.fromResult) pieDataDecoder
        }


fetchHistogramData : DashboardId -> Int -> Cmd Msg
fetchHistogramData dashboardId widgetId =
    Http.post
        { url = "http://127.0.0.1:6969/fetch-widget-data"
        , body =
            Http.jsonBody
                (Json.Encode.object [ ( "dashboard_id", Dashboard.idEncoder dashboardId ), ( "widget_id", Json.Encode.int widgetId ) ])
        , expect = Http.expectJson (HistogramDataReceived << RemoteData.fromResult) histogramDataDecoder
        }


getDataSourceTables : DashboardId -> Cmd Msg
getDataSourceTables dashboardId =
    Http.post
        { url = "http://127.0.0.1:6969/get-data-source-tables"
        , body =
            Http.jsonBody
                (Json.Encode.object [ ( "dashboard_id", Dashboard.idEncoder dashboardId ) ])
        , expect = Http.expectJson (TablesReceived << RemoteData.fromResult) tablesDecoder
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
