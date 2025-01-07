module Page.ListDashboards exposing (Model, Msg, init, update, view)

import Dashboard exposing (..)
import RemoteData exposing (WebData)
import Http exposing (..)
import Json.Decode exposing (list)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (href, type_)
import Error exposing (buildErrorMessage)
import Json.Encode exposing (object)



type alias Model =
    { dashboards : WebData (List Dashboard)
    , deleteError : Maybe String
    }

type Msg
    = FetchDashboards
    | DashboardsReceived (WebData (List Dashboard))
    | DeleteDashboard DashboardId
    | DashboardDeleted (Result Http.Error (List Dashboard))

init : ( Model, Cmd Msg )
init =
    ( initialModel, fetchDashboards )

initialModel : Model
initialModel =
    { dashboards = RemoteData.NotAsked
    , deleteError = Nothing
    }

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

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchDashboards ->
            ( { model | dashboards = RemoteData.Loading }, fetchDashboards )

        DashboardsReceived response ->
            ( { model | dashboards = response }, Cmd.none )

        DeleteDashboard dashboardId ->
            ( model, deleteDashboard dashboardId )

        DashboardDeleted (Ok dashboards) ->
            ( { model | dashboards = RemoteData.Success dashboards, deleteError = Nothing }, Cmd.none )

        DashboardDeleted (Err httpError) ->
            ( { model | deleteError = Just (buildErrorMessage httpError) }, Cmd.none )

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick FetchDashboards ]
            [ text "Refresh dashboards" ]
        , viewDashboards model.dashboards
        , viewDeleteError model.deleteError
        ]

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
    let
        dashboardPath =
            "/dashboards/" ++ (idToString dashboard.dashboard_id)
    in
    tr []
        [ 
         td []
            [ text dashboard.title ]
        , td []
            [ a [ href (dashboardPath ++ "/edit") ] [ text "Edit" ] ]
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