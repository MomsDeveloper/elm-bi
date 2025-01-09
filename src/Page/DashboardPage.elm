module Page.DashboardPage exposing (..)
import Dashboard exposing (..)
import Browser.Navigation as Nav
import RemoteData exposing (WebData)
import Html exposing (..)
import Json.Encode 
import Http exposing (..)
import Error exposing (buildErrorMessage)
import Html.Attributes exposing (href, rel, class, value, disabled)
import Html.Events exposing (onClick, onInput)
import Widgets.PieChart
import Widgets.Histogram
import List.Extra 


type alias Model =
    { navKey : Nav.Key
    , dashboard : WebData Dashboard
    , widgetsData : List (WebData WidgetData)
    , tables : WebData (List Table)
    , showAddWidgetForm : Bool
    , selectedTable : Table
    , formButtonsDisabled : Bool
    , newWidget : AddWidget
    }

type alias AddWidget =
    { 
      widget_type : String
    , widget_id : Int
    , title : String
    , table : String
    , data_column : String
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
    , newWidget = emptyWidget
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DashboardReceived response ->
            ( { model | dashboard = response , widgetsData = [] , tables = RemoteData.NotAsked }
            , case response of
                RemoteData.Success dashboard -> getDataSourceTables dashboard.dashboard_id
                _ -> Cmd.none
            )
        
        PieDataReceived response ->
            ( { model | widgetsData = model.widgetsData ++ [RemoteData.map (\pieData -> Piedata pieData) response] }, Cmd.none )
        HistogramDataReceived response ->
            ( { model | widgetsData = model.widgetsData ++ [RemoteData.map (\histogramData -> Histogramdata histogramData) response] }, Cmd.none )

        TablesReceived response ->
            case response of
                RemoteData.Success (head :: _) ->
                    let
                        newWidget = model.newWidget
                        updatedWidget = { newWidget | table = head.name }
                    in
                    ( { model | tables = response, selectedTable = head, newWidget = updatedWidget }, fetchWidgetsData model )
                _ ->
                    ( { model | tables = response }, fetchWidgetsData model )
        
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
            let
                selectedTable =
                    List.Extra.find (\table -> table.name == tableName) (RemoteData.withDefault [] model.tables)
                
                newWidget = model.newWidget
                updatedWidget = { newWidget | table = tableName }
            in
            case selectedTable of
                Just table ->
                    ( { model | selectedTable = table, formButtonsDisabled = False, newWidget = updatedWidget }, Cmd.none )
                Nothing ->
                    ( model, Cmd.none )

        ColumnToggled columnName ->
            let
                newWidget = model.newWidget
                updatedWidget = { newWidget | data_column = columnName }
            in
            if model.formButtonsDisabled then
                ( { model | formButtonsDisabled = False, newWidget = updatedWidget }, Cmd.none )
            else
                ( { model | formButtonsDisabled = True, newWidget = updatedWidget }, Cmd.none )

        WidgetTypeSelected widgetType ->
            let
                newWidget = model.newWidget
                updatedWidget = { newWidget | widget_type = widgetType }
            in
            ( { model | newWidget = updatedWidget }, Cmd.none )

        AddNewWidget ->
            case model.dashboard of
                RemoteData.Success dashboardData ->
                    ( { model | showAddWidgetForm = False , newWidget = emptyWidget, formButtonsDisabled = False }, addWidget model.newWidget dashboardData.dashboard_id )
                _ ->
                    ( model, Cmd.none )

emptyWidget : AddWidget
emptyWidget = { widget_type = "PieChart", widget_id = 0, title = "", table = "", data_column = "" }

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
            , href "/styles/main.css"] []
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
                , div [] [ 
                select 
                    [ class "table-dropdown", onInput (FormChanged << WidgetTypeSelected) ]
                    [ option [ value "PieChart" ] [ text "Pie Chart" ] 
                    , option [ value "Histogram" ] [ text "Histogram" ] ]
                ]
                , select
                    [ class "table-dropdown", onInput (FormChanged << TableSelected) ]
                    (List.map (\table -> option [ value table.name ] [ text table.name ]) tableList)
                , div [] [ text "Select columns:" ]
                , viewTableColumns (model)
                , button [ class "add-widget-button", onClick (FormChanged AddNewWidget)] [ text "Add Widget" ]
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
                        , disabled (model.newWidget.data_column /= col.name && model.formButtonsDisabled)
                        ] 
                        [ text col.name ]
                    ]
            )
            model.selectedTable.columns
        )


viewAddWidgetButton :  Html Msg
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
                    ([ ] ++ List.map viewWidget widgetsData)
                ]
        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)


viewWidget : WebData WidgetData -> Html Msg
viewWidget  widgetData =
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
        , body = Http.jsonBody 
            (Json.Encode.object [ ("dashboard_id", Dashboard.idEncoder dashboardId) ])
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
        , body = Http.jsonBody 
            (Json.Encode.object [ ("dashboard_id", Dashboard.idEncoder dashboardId), ("widget_id", Json.Encode.int widgetId) ])
        , expect = Http.expectJson (PieDataReceived << RemoteData.fromResult) pieDataDecoder
        }

fetchHistogramData : DashboardId -> Int -> Cmd Msg
fetchHistogramData dashboardId widgetId =
    Http.post
        { url = "http://127.0.0.1:6969/fetch-widget-data"
        , body = Http.jsonBody 
            (Json.Encode.object [ ("dashboard_id", Dashboard.idEncoder dashboardId), ("widget_id", Json.Encode.int widgetId) ])
        , expect = Http.expectJson (HistogramDataReceived << RemoteData.fromResult) histogramDataDecoder
        }

getDataSourceTables : DashboardId -> Cmd Msg
getDataSourceTables dashboardId =
    Http.post
        { url = "http://127.0.0.1:6969/get-data-source-tables"
        , body = Http.jsonBody 
            (Json.Encode.object [ ("dashboard_id", Dashboard.idEncoder dashboardId) ])
        , expect = Http.expectJson (TablesReceived << RemoteData.fromResult) tablesDecoder
        }
    
addWidget : AddWidget -> DashboardId -> Cmd Msg
addWidget widget dashboardId =
    Http.post
        { url = "http://127.0.0.1:6969/add-widget"
        , body = Http.jsonBody 
            (Json.Encode.object [ ("dashboard_id", Dashboard.idEncoder dashboardId), ("widget", addWidgetEncoder widget) ])
        , expect = Http.expectJson (DashboardReceived << RemoteData.fromResult) dashboardDecoder
        }

addWidgetEncoder : AddWidget -> Json.Encode.Value
addWidgetEncoder widget =
    Json.Encode.object 
        [ ("widget_type", Json.Encode.string widget.widget_type)
        , ("widget_id", Json.Encode.int widget.widget_id)
        , ("title", Json.Encode.string widget.title)
        , ("table", Json.Encode.string widget.table)
        , ("data_column", Json.Encode.string widget.data_column)
        ]