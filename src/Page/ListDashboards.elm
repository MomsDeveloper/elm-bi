module Page.ListDashboards exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Components.AddDashboardForm as AddDashboardForm exposing (Model)
import Error exposing (buildErrorMessage)
import Html exposing (Html, button, div, h1, h3, p, text)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, int, list, map2, string)
import Json.Encode
import Models.Dashboard as Dashboard exposing (Dashboard, DashboardId, dashboardDecoder, dashboardEncoder)
import Platform.Cmd as Cmd
import RemoteData exposing (WebData)
import Route


type alias Model =
    { dashboards : WebData (List DashboardInfo)
    , addDashboardForm : AddDashboardForm.Model
    , showAddDashboardForm : Bool
    , deleteError : Maybe String
    , createError : Maybe String
    , navKey : Nav.Key
    }


type Msg
    = DashboardsReceived (WebData (List DashboardInfo))
    | DeleteDashboard DashboardId
    | DashboardDeleted (WebData (List DashboardInfo))
    | ShowForm
    | FormChanged AddDashboardForm.Msg
    | DashboardCreated (Result Http.Error Dashboard)
    | GoToDashboard DashboardId


type alias DashboardInfo =
    { dashboard_id : DashboardId
    , title : String
    }


dashBoardsInfoDecoder : Decoder (List DashboardInfo)
dashBoardsInfoDecoder =
    list
        (map2
            (\dashboard_id title ->
                { dashboard_id = Dashboard.DashboardId dashboard_id
                , title = title
                }
            )
            (field "dashboard_id" int)
            (field "title" string)
        )


init : Nav.Key -> ( Model, Cmd Msg )
init navKey =
    ( initialModel navKey, fetchDashboards )


initialModel : Nav.Key -> Model
initialModel navKey =
    { dashboards = RemoteData.NotAsked
    , addDashboardForm = AddDashboardForm.init
    , showAddDashboardForm = False
    , deleteError = Nothing
    , createError = Nothing
    , navKey = navKey
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DashboardsReceived response ->
            ( { model | dashboards = response }, Cmd.none )

        DeleteDashboard dashboardId ->
            ( model, deleteDashboard dashboardId )

        DashboardDeleted response ->
            ( { model | dashboards = response, deleteError = Nothing }, Cmd.none )

        DashboardCreated (Ok dashboard) ->
            ( model, Route.pushUrl (Route.Dashboard dashboard.dashboard_id) model.navKey )

        ShowForm ->
            ( { model | showAddDashboardForm = True }, Cmd.none )

        FormChanged formMsg ->
            case formMsg of
                AddDashboardForm.Cancel ->
                    ( { model | showAddDashboardForm = False }, Cmd.none )

                AddDashboardForm.AddNewDashboard ->
                    ( { model | showAddDashboardForm = False }, create_dashboard model.addDashboardForm.dashboard )

                _ ->
                    let
                        updatedForm =
                            AddDashboardForm.update formMsg model.addDashboardForm
                    in
                    ( { model | addDashboardForm = updatedForm }, Cmd.none )

        DashboardCreated (Err httpError) ->
            ( { model | createError = Just (buildErrorMessage httpError) }, Cmd.none )

        GoToDashboard dashboardId ->
            ( model, Route.pushUrl (Route.Dashboard dashboardId) model.navKey )


view : Model -> Html Msg
view model =
    div []
        [ viewHeader
        , viewDashboards model.dashboards
        , if model.showAddDashboardForm then
            Html.map FormChanged (AddDashboardForm.view model.addDashboardForm)

          else
            text ""
        , viewDeleteError model.deleteError
        , viewAddError model.createError
        ]


viewHeader : Html Msg
viewHeader =
    div [ class "dashboard-header" ]
        [ h1 [] [ text "Dashboards" ]
        , p [] [ text "Manage your dashboards below. Click '+' to add a new dashboard." ]
        ]


viewAddDashboardButton : Html Msg
viewAddDashboardButton =
    div
        [ class "dashboard-square add"
        , onClick ShowForm
        ]
        [ text "+" ]


viewDashboards : WebData (List DashboardInfo) -> Html Msg
viewDashboards dashboards =
    case dashboards of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success actualDashboards ->
            div [ class "dashboard-container" ]
                (viewAddDashboardButton :: List.map viewDashboard actualDashboards)

        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)


viewDashboard : DashboardInfo -> Html Msg
viewDashboard dashboard =
    div [ class "dashboard-square", onClick (GoToDashboard dashboard.dashboard_id) ]
        [ text dashboard.title
        , button
            [ class "delete-button"
            , type_ "button"
            , Html.Events.stopPropagationOn "click" (Json.Decode.succeed ( DeleteDashboard dashboard.dashboard_id, True ))
            ]
            [ text "✖" ]
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
        , body = Http.jsonBody (Json.Encode.object [])
        , expect =
            dashBoardsInfoDecoder
                |> Http.expectJson (RemoteData.fromResult >> DashboardsReceived)
        }


deleteDashboard : DashboardId -> Cmd Msg
deleteDashboard dashboardId =
    Http.post
        { url = "http://127.0.0.1:6969/delete-dashboard"
        , body =
            Http.jsonBody
                (Json.Encode.object [ ( "dashboard_id", Dashboard.idEncoder dashboardId ) ])
        , expect =
            dashBoardsInfoDecoder
                |> Http.expectJson (RemoteData.fromResult >> DashboardDeleted)
        }


create_dashboard : Dashboard -> Cmd Msg
create_dashboard dashboard =
    Http.post
        { url = "http://127.0.0.1:6969/create-dashboard"
        , body =
            Http.jsonBody
                (dashboardEncoder dashboard)
        , expect = Http.expectJson DashboardCreated dashboardDecoder
        }
