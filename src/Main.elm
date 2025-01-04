-- Press a button to send a GET request for random quotes.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/json.html
--
module Main exposing (main)

import Browser
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
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL

type Loading a =
  Loading
  | Finished a

type alias Model =
    { dashboards : Loading (Result String (List Dashboard)) }


init : () -> (Model, Cmd Msg)
init _ =
  (Model Loading, get_dashboards)


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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotDashboards (Ok dashboard) ->
      -- (model (Finished (Ok dashboard)), Cmd.none)
      ({ model | dashboards = Finished (Ok dashboard) }, Cmd.none)

    GotDashboards (Err error) ->
      let
          _ = Debug.log "HTTP Error" error
      in
      (Model (Finished (Err (errorToString error))), Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  case model.dashboards of
    Loading ->
      div [] [ text "Loading..." ]

    Finished (Ok (dashboard :: _) ) ->
      div [] [ text dashboard.title ]

    Finished (Ok [] ) ->
      div [] [ text "nothing" ]
    
    Finished (Err error) ->
      div [] [ text error ]




-- HTTP

get_dashboards : Cmd Msg
get_dashboards =
  Http.post
    { url = "http://127.0.0.1:6969/get-dashboards"
    , body = Http.emptyBody
    , expect = Http.expectJson GotDashboards (list dashboardDecoder)
    }

