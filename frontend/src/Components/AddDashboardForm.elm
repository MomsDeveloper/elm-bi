module Components.AddDashboardForm exposing (Model, Msg(..), emptyDashboard, init, update, view)

import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (class, disabled, type_, value)
import Html.Events exposing (onClick, onInput)
import Models.Dashboard exposing (Dashboard)


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


view : Model -> Html Msg
view model =
    let
        dashboard =
            model.dashboard
    in
    div [ class "form" ]
        [ div [ class "form-content" ]
            [ div [ class "form-group" ]
                [ label [] [ text "Password" ]
                , input
                    [ type_ "password"
                    , onInput UpdatePassword
                    , value dashboard.dataSource.password
                    ]
                    []
                ]
            , div [ class "form-group" ]
                [ label [] [ text "Username" ]
                , input
                    [ type_ "text"
                    , onInput UpdateUsername
                    , value dashboard.dataSource.username
                    ]
                    []
                ]
            , div [ class "form-group" ]
                [ label [] [ text "Host" ]
                , input
                    [ type_ "text"
                    , onInput UpdateHost
                    , value dashboard.dataSource.host
                    ]
                    []
                ]
            , div [ class "form-group" ]
                [ label [] [ text "Port" ]
                , input
                    [ type_ "number"
                    , onInput (UpdatePortNumber << Maybe.withDefault 0 << String.toInt)
                    , value (String.fromInt dashboard.dataSource.portNumber)
                    ]
                    []
                ]
            , div [ class "form-group" ]
                [ label [] [ text "Database" ]
                , input
                    [ type_ "text"
                    , onInput UpdateDatabase
                    , value dashboard.dataSource.database
                    ]
                    []
                ]
            , div [ class "form-group" ]
                [ label [] [ text "Dashboard Title" ]
                , input
                    [ type_ "text"
                    , onInput UpdateTitle
                    , value dashboard.title
                    ]
                    []
                ]
            , div [ class "form-buttons" ]
                [ button [ onClick Cancel ] [ text "Cancel" ]
                , if checkIfValid model then
                    button [ onClick AddNewDashboard ] [ text "Add" ]

                  else
                    button [ disabled True ] [ text "Add" ]
                ]
            ]
        ]


checkIfValid : Model -> Bool
checkIfValid model =
    let
        isValid =
            model.dashboard.title
                /= ""
                && model.dashboard.dataSource.host
                /= ""
                && model.dashboard.dataSource.portNumber
                /= 0
                && model.dashboard.dataSource.username
                /= ""
                && model.dashboard.dataSource.password
                /= ""
                && model.dashboard.dataSource.database
                /= ""
    in
    isValid


emptyDashboard : Dashboard
emptyDashboard =
    { dashboard_id = Models.Dashboard.DashboardId -1
    , title = ""
    , dataSource = { host = "", portNumber = 0, username = "", password = "", database = "", tables = [] }
    , widgets = []
    }
