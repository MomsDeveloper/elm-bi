module Page.DashboardPage exposing (..)
import Dashboard exposing (..)
import Browser.Navigation as Nav
import RemoteData exposing (WebData)
import Html exposing (..)
import Json.Encode 
import Http exposing (..)
import Error exposing (buildErrorMessage)


type alias Model =
    { navKey : Nav.Key
    , dashboard : WebData Dashboard
    }

type Msg
    = DashboardReceived (WebData Dashboard)

init : DashboardId -> Nav.Key -> ( Model, Cmd Msg )
init dashboardId navKey =
    ( initialModel navKey, getDashboard dashboardId )


initialModel : Nav.Key -> Model
initialModel navKey =
    { navKey = navKey
    , dashboard = RemoteData.Loading
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DashboardReceived response ->
            ( { model | dashboard = response }, Cmd.none )

view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Widgets" ]
        , viewDashboard model.dashboard
        ]

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
                , ul [] (List.map viewWidget dashboardData.widgets)
                ]
        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)

viewWidget : Widget -> Html Msg
viewWidget widget =
    case widget of
        Pie pie ->
            div []
                [ h3 [] [ text pie.title ]
                , text pie.table
                , text pie.x_column
                , ul [] (List.map viewPieSection (Maybe.withDefault [] pie.sections))
                ]

        _ ->
            text "Unknown widget type"

viewPieSection : PieSection -> Html Msg
viewPieSection pieSection =
    div []
        [ h3 [] [ text pieSection.title ]
        , text (String.fromFloat pieSection.percentage)
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
        , body = Http.jsonBody 
            (Json.Encode.object [ ("dashboard_id", Dashboard.idEncoder dashboardId) ])
        , expect = Http.expectJson (DashboardReceived << RemoteData.fromResult) dashboardDecoder
        }