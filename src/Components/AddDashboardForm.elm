module Components.AddDashboardForm exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models.Dashboard exposing (..)
import Models.DataSource exposing (..)
import Models.Widgets exposing (..)


type alias Model =
    { dashboard : Dashboard
    }


type Msg
    = UpdateTitle String
    | UpdatePassword String
    | UpdateUsername String
    | UpdateHost String
    | UpdatePortNumber Int
    | UpdateDatabase String
    | Cancel
    | AddNewDashboard


init : Model
init =
    { dashboard = emptyDashboard
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdatePassword password ->
            let
                dataSource =
                    model.dashboard.dataSource

                oldDashboard =
                    model.dashboard

                updatedDataSource =
                    { dataSource | password = password }

                updatedDashboard =
                    { oldDashboard | dataSource = updatedDataSource }
            in
            { model | dashboard = updatedDashboard }

        UpdateUsername username ->
            let
                dataSource =
                    model.dashboard.dataSource

                oldDashboard =
                    model.dashboard

                updatedDataSource =
                    { dataSource | username = username }

                updatedDashboard =
                    { oldDashboard | dataSource = updatedDataSource }
            in
            { model | dashboard = updatedDashboard }

        UpdateHost host ->
            let
                dataSource =
                    model.dashboard.dataSource

                oldDashboard =
                    model.dashboard

                updatedDataSource =
                    { dataSource | host = host }

                updatedDashboard =
                    { oldDashboard | dataSource = updatedDataSource }
            in
            { model | dashboard = updatedDashboard }

        UpdatePortNumber portNumber ->
            let
                dataSource =
                    model.dashboard.dataSource

                oldDashboard =
                    model.dashboard

                updatedDataSource =
                    { dataSource | portNumber = portNumber }

                updatedDashboard =
                    { oldDashboard | dataSource = updatedDataSource }
            in
            { model | dashboard = updatedDashboard }

        UpdateDatabase database ->
            let
                dataSource =
                    model.dashboard.dataSource

                oldDashboard =
                    model.dashboard

                updatedDataSource =
                    { dataSource | database = database }

                updatedDashboard =
                    { oldDashboard | dataSource = updatedDataSource }
            in
            { model | dashboard = updatedDashboard }

        UpdateTitle title ->
            let
                oldDashboard =
                    model.dashboard

                updatedDashboard =
                    { oldDashboard | title = title }
            in
            { model | dashboard = updatedDashboard }

        Cancel ->
            init

        AddNewDashboard ->
            model


view : Html Msg
view =
    div [ class "form" ]
        [ div [ class "form-content" ]
            [ div [ class "form-group" ]
                [ label [] [ text "Password" ]
                , input
                    [ type_ "password"
                    , onInput UpdatePassword
                    ]
                    []
                ]
            , div [ class "form-group" ]
                [ label [] [ text "Username" ]
                , input
                    [ type_ "text"
                    , onInput UpdateUsername
                    ]
                    []
                ]
            , div [ class "form-group" ]
                [ label [] [ text "Host" ]
                , input
                    [ type_ "text"
                    , onInput UpdateHost
                    ]
                    []
                ]
            , div [ class "form-group" ]
                [ label [] [ text "Port" ]
                , input
                    [ type_ "number"
                    , onInput (UpdatePortNumber << Maybe.withDefault 0 << String.toInt)
                    ]
                    []
                ]
            , div [ class "form-group" ]
                [ label [] [ text "Database" ]
                , input
                    [ type_ "text"
                    , onInput UpdateDatabase
                    ]
                    []
                ]
            , div [ class "form-group" ]
                [ label [] [ text "Dashboard Title" ]
                , input
                    [ type_ "text"
                    , onInput UpdateTitle
                    ]
                    []
                ]
            , div [ class "form-buttons" ]
                [ button [ onClick Cancel ] [ text "Cancel" ]
                , button [ onClick AddNewDashboard ] [ text "Create" ]
                ]
            ]
        ]


emptyDashboard : Dashboard
emptyDashboard =
    { dashboard_id = DashboardId -1
    , title = ""
    , dataSource = { host = "", portNumber = 0, username = "", password = "", database = "", tables = [] }
    , widgets = []
    }
