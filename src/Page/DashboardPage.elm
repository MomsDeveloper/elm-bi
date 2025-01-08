module Page.DashboardPage exposing (..)
import Dashboard exposing (..)
import Browser.Navigation as Nav
import RemoteData exposing (WebData)
import Html exposing (..)
import Json.Encode 
import Http exposing (..)
import Error exposing (buildErrorMessage)
import Html.Attributes exposing (href, rel, class)
import Widgets.PieChart
import Widgets.Histogram


type alias Model =
    { navKey : Nav.Key
    , dashboard : WebData Dashboard
    , widgetsData : List (WebData WidgetData)
    }

type Msg
    = DashboardReceived (WebData Dashboard)
    | PieDataReceived (WebData (List PieData))
    | HistogramDataReceived (WebData (List Float))

init : DashboardId -> Nav.Key -> ( Model, Cmd Msg )
init dashboardId navKey =
    ( initialModel navKey, getDashboard dashboardId )


initialModel : Nav.Key -> Model
initialModel navKey =
    { navKey = navKey
    , dashboard = RemoteData.Loading
    , widgetsData = []
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DashboardReceived response ->
            let
                updatedModel =
                    { model | dashboard = response }
            in
            ( updatedModel, fetchWidgetsData updatedModel )
        
        PieDataReceived response ->
            ( { model | widgetsData = model.widgetsData ++ [RemoteData.map (\pieData -> Piedata pieData) response] }, Cmd.none )
        HistogramDataReceived response ->
            ( { model | widgetsData = model.widgetsData ++ [RemoteData.map (\histogramData -> Histogramdata histogramData) response] }, Cmd.none )

view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Widgets" ]
        , viewDashboard model.dashboard model.widgetsData
        , node "link" 
            [ rel "stylesheet"
            , href "/styles/main.css"] []
        ]

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

