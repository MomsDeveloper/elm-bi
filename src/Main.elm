-- Press a button to send a GET request for random quotes.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/json.html
--
module Main exposing (main)

import Browser
import Url exposing (Url)
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (list)

-- import Chart exposing (Chart)
-- import Chart.Pie exposing (slice)
-- import Svg.Attributes exposing (style)
-- import Chart exposing (Chart)
-- import Chart.Bar exposing (bars)

import Types exposing (..)
import Decoders exposing (..)

-- MAIN


main =
  Browser.application
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }



-- MODEL

type Loading a =
  Loading
  | Finished a

type alias Model =
    { dashboards : Loading (Result String (List Dashboard)) 
    , currentDashboard : Maybe Int
    , navKey : Navigation.Key
    , url : Url
    }

init : () -> Url -> Navigation.Key -> (Model, Cmd Msg)
init _ url key =
  ( { dashboards = Loading
    , currentDashboard = Nothing
    , navKey = key
    , url = url
    }
  , get_dashboards
  )

-- UPDATE

errorToString : Http.Error -> String
errorToString error =
  case error of
    Http.BadUrl url ->
      "Bad URL: " ++ url

    Http.Timeout ->
      "Request timed out"

    Http.NetworkError ->
      "Network error"

    Http.BadStatus status ->
      "Bad status: " ++ String.fromInt status

    Http.BadBody body ->
      "Bad body: " ++ body


type Msg 
    = GotDashboards (Result Http.Error (List Dashboard))
    | NavigateToDashboard Int
    | NavigateToHome
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotDashboards (Ok dashboard) ->
      ({ model | dashboards = Finished (Ok dashboard) }, Cmd.none)

    GotDashboards (Err error) ->
      let
          _ = Debug.log "HTTP Error" error
      in
      ({ model | dashboards = Finished (Err (errorToString error)) }, Cmd.none)
        
    NavigateToDashboard dashboard_id ->
      let
          newUrl = "/dashboard/" ++ String.fromInt dashboard_id
      in
      -- (model, Navigation.pushUrl model.navKey newUrl)
      ({ model | currentDashboard = Just dashboard_id }, Navigation.pushUrl model.navKey newUrl)
    
    NavigateToHome ->
      ({ model | currentDashboard = Nothing }, Navigation.pushUrl model.navKey "/")

    UrlChanged url ->
      ({ model | url = url }, Cmd.none)

    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          (model, Navigation.pushUrl model.navKey (Url.toString url))

        Browser.External href ->
          (model, Navigation.load href)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW

renderDashboardSquare : Dashboard -> Html Msg
renderDashboardSquare dashboard =
    div 
        [ style "width" "100px"
        , style "height" "100px"
        , style "border" "1px solid black"
        , style "margin" "10px"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "cursor" "pointer" -- Makes it clear the div is clickable
        , onClick (NavigateToDashboard dashboard.dashboard_id)
        ]
        [ text dashboard.title ]

renderAddDashboardSquare : Html Msg
renderAddDashboardSquare =
    div 
        [ style "width" "100px"
        , style "height" "100px"
        , style "border" "1px solid black"
        , style "margin" "10px"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "cursor" "pointer" -- Makes it clear the div is clickable
        , onClick (NavigateToDashboard 0)
        ]
        [ text "+" ]

view : Model -> Browser.Document Msg
view model =
  { title = 
      case model.currentDashboard of
        Just dashboard_id ->
          "Dashboard " ++ String.fromInt dashboard_id

        Nothing ->
          "Dashboard"
  , body =
      case model.currentDashboard of
        Just dashboard_id ->
          -- [ div [] [ text ("Dashboard " ++ String.fromInt dashboard_id) ] ]
          [div [] [ div [] 
            [ text "Dashboard " 
            , text (String.fromInt dashboard_id) 
            , button [ onClick (NavigateToHome) ] [ text "Back"]
            ]
          ]
          ]

        Nothing ->
          case model.dashboards of
            Loading ->
              [ div [] [ text "Loading..." ] ]

            Finished (Ok dashboards) ->
              [ div [] (List.map renderDashboardSquare dashboards) 
              , renderAddDashboardSquare
              ]
            
            Finished (Err error) ->
              [ div [] [ text error ] ]
  }



-- HTTP

get_dashboards : Cmd Msg
get_dashboards =
  Http.post
    { url = "http://127.0.0.1:6969/get-dashboards"
    , body = Http.emptyBody
    , expect = Http.expectJson GotDashboards (list dashboardDecoder)
    }

