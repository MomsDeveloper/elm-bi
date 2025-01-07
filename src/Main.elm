module Main exposing (..)

import Browser
import Browser exposing (Document, UrlRequest)
import Html exposing (..)
import Url exposing (Url)
import Browser.Navigation as Nav
import Page.ListDashboards as ListDashboards
import Route exposing (Route)
import Page.EditDashboard as EditDashboard

main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }

type alias Model =
    { route : Route
    , page : Page
    , navKey : Nav.Key
    }


type Page
    = NotFoundPage
    | ListPage ListDashboards.Model
    | EditDashboardPage EditDashboard.Model

type Msg
    = ListPageMsg ListDashboards.Msg
    | LinkClicked UrlRequest
    | UrlChanged Url
    | EditDashboardPageMsg EditDashboard.Msg

init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        model =
            { route = Route.parseUrl url
            , page = NotFoundPage
            , navKey = navKey
            }
    in
    initCurrentPage ( model, Cmd.none )


initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, existingCmds ) =
    let
        ( currentPage, mappedPageCmds ) =
            case model.route of
                Route.NotFound ->
                    ( NotFoundPage, Cmd.none )

                Route.Dashboards ->
                    let
                        ( pageModel, pageCmds ) =
                            ListDashboards.init
                    in
                    ( ListPage pageModel, Cmd.map ListPageMsg pageCmds )

                Route.EditDashboard dashboardId ->
                    let
                        ( pageModel, pageCmd ) =
                            EditDashboard.init dashboardId model.navKey
                    in
                    ( EditDashboardPage pageModel, Cmd.map EditDashboardPageMsg pageCmd )

                Route.Dashboard dashboardId ->
                    ( NotFoundPage, Cmd.none )
    in
    ( { model | page = currentPage }
    , Cmd.batch [ existingCmds, mappedPageCmds ]
    )

view : Model -> Document Msg
view model =
    { title = "Post App"
    , body = [ currentView model ]
    }


currentView : Model -> Html Msg
currentView model =
    case model.page of
        NotFoundPage ->
            notFoundView

        ListPage pageModel ->
            ListDashboards.view pageModel
                |> Html.map ListPageMsg

        EditDashboardPage pageModel ->
            EditDashboard.view pageModel
                |> Html.map EditDashboardPageMsg
    

notFoundView : Html msg
notFoundView =
    h3 [] [ text "Oops! The page you requested was not found!" ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( ListPageMsg subMsg, ListPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    ListDashboards.update subMsg pageModel
            in
            ( { model | page = ListPage updatedPageModel }
            , Cmd.map ListPageMsg updatedCmd
            )
        
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.navKey (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        ( UrlChanged url, _ ) ->
            let
                newRoute =
                    Route.parseUrl url
            in
            ( { model | route = newRoute }, Cmd.none )
                |> initCurrentPage

        ( EditDashboardPageMsg subMsg, EditDashboardPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    EditDashboard.update subMsg pageModel
            in
            ( { model | page = EditDashboardPage updatedPageModel }
            , Cmd.map EditDashboardPageMsg updatedCmd
            )

        ( _, _ ) ->
            ( model, Cmd.none )