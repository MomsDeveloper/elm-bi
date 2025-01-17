module Components.AddWidgetForm exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, button, div, h3, input, option, select, text)
import Html.Attributes exposing (class, disabled, type_, value)
import Html.Events exposing (onClick, onInput)
import List.Extra
import Models.DataSource exposing (Table)
import Models.Widgets exposing (Widget(..))


type alias Model =
    { widget : Widget
    , selectedTable : Table
    , tables : List Table
    , isWidgetTypeSelected : Bool
    }


type Msg
    = TableSelected String
    | WidgetTypeSelected String
    | WidgetTitleChanged String
    | ColumnToggled String
    | Cancel
    | AddNewWidget


init : List Table -> Model
init tables =
    initialModel tables


initialModel : List Table -> Model
initialModel tables =
    { widget = emptyWidget (List.head tables |> Maybe.withDefault (Table "" []))
    , selectedTable = List.head tables |> Maybe.withDefault (Table "" [])
    , tables = tables
    , isWidgetTypeSelected = False
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Cancel ->
            initialModel model.tables

        TableSelected tableName ->
            updateTableSelection tableName model

        ColumnToggled columnName ->
            toggleColumn columnName model

        WidgetTypeSelected widgetType ->
            let
                widget =
                    model.widget
            in
            { model | widget = updateWidgetType widgetType widget, isWidgetTypeSelected = True }

        WidgetTitleChanged title ->
            let
                updatedWidget =
                    case model.widget of
                        Pie data ->
                            Pie { data | title = title }

                        Histogram data ->
                            Histogram { data | title = title }
            in
            { model | widget = updatedWidget }

        AddNewWidget ->
            model


updateTableSelection : String -> Model -> Model
updateTableSelection tableName model =
    let
        selectedTable =
            List.Extra.find (\table -> table.name == tableName) model.tables

        updatedWidget =
            case model.widget of
                Pie data ->
                    Pie { data | table = tableName }

                Histogram data ->
                    Histogram { data | table = tableName }
    in
    case selectedTable of
        Just table ->
            { model | selectedTable = table, widget = updatedWidget }

        Nothing ->
            model


updateWidgetType : String -> Widget -> Widget
updateWidgetType widgetType widget =
    case widget of
        Pie { title, table, data_column } ->
            case widgetType of
                "PieChart" ->
                    Pie { widget_id = 0, title = title, table = table, data_column = data_column, data = [] }

                "Histogram" ->
                    Histogram { widget_id = 0, title = title, table = table, data_column = data_column, data = [] }

                _ ->
                    Pie { widget_id = 0, title = title, table = table, data_column = data_column, data = [] }

        Histogram { title, table, data_column } ->
            case widgetType of
                "PieChart" ->
                    Pie { widget_id = 0, title = title, table = table, data_column = data_column, data = [] }

                "Histogram" ->
                    Histogram { widget_id = 0, title = title, table = table, data_column = data_column, data = [] }

                _ ->
                    Pie { widget_id = 0, title = title, table = table, data_column = data_column, data = [] }


toggleColumn : String -> Model -> Model
toggleColumn columnName model =
    let
        updatedWidget =
            case model.widget of
                Pie data ->
                    Pie { data | data_column = columnName }

                Histogram data ->
                    Histogram { data | data_column = columnName }
    in
    { model | widget = updatedWidget }


emptyWidget : Table -> Widget
emptyWidget table =
    let
        tableColumn =
            table.columns |> List.head |> Maybe.map .name |> Maybe.withDefault ""
    in
    Pie { widget_id = 0, title = "", table = table.name, data_column = tableColumn, data = [] }


view : Model -> Html Msg
view model =
    div [ class "form" ]
        [ h3 [] [ text "Add Widget" ]
        , div [ class "form-content" ]
            [ div [ class "form-group" ]
                [ div [] [ text "Your widget title:" ]
                , input
                    [ type_ "text"
                    , onInput WidgetTitleChanged
                    ]
                    []
                ]
            , div [ class "form-group" ]
                [ select
                    [ onInput WidgetTypeSelected ]
                    [ if not model.isWidgetTypeSelected then
                        option [ value "PieChart" ] [ text "Choose a widget" ]

                      else
                        text ""
                    , option [ value "PieChart" ] [ text "Pie Chart" ]
                    , option [ value "Histogram" ] [ text "Histogram" ]
                    ]
                ]
            , if model.isWidgetTypeSelected then
                div [ class "form-group" ]
                    [ div [] [ text "Select table:" ]
                    , select
                        [ onInput TableSelected ]
                        (List.map (\table -> option [ value table.name ] [ text table.name ]) model.tables)
                    , div [] [ text "Select columns:" ]
                    , viewTableColumns model.selectedTable
                    ]

              else
                text ""
            , div [ class "form-buttons" ]
                [ button [ onClick Cancel ] [ text "Cancel" ]
                , if checkIfValid model then
                    button [ onClick AddNewWidget ] [ text "Add Widget" ]

                  else
                    button [ disabled True ] [ text "Add Widget" ]
                ]
            ]
        ]


viewTableColumns : Table -> Html Msg
viewTableColumns table =
    select [ onInput ColumnToggled ]
        (List.map (\col -> option [ value col.name ] [ text col.name ]) table.columns)


checkIfValid : Model -> Bool
checkIfValid model =
    let
        title =
            case model.widget of
                Pie data ->
                    data.title

                Histogram data ->
                    data.title
    in
    String.length title < 10 && title /= "" && model.isWidgetTypeSelected
