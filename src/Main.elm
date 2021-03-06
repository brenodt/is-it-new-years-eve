module Main exposing (..)

import Browser
import Html exposing(..)
import Task exposing(Task)
import Time

-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
type alias Model =
  { zone : Time.Zone 
  , time : Time.Posix
  , timeLeft : Time.Posix
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0) (Time.millisToPosix 0)
  , Task.perform AdjustTimeZone Time.here
  )

-- UPDATE
type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model | time = newTime, timeLeft = timeLeftForNewYear model.zone newTime }
      , Cmd.none
      )

    AdjustTimeZone newZone ->
      ( { model | zone = newZone }
      , Cmd.none
      )

-- 1s == 1000ms
hour = 1000 * 60 * 60
year = hour * 24 * 365 + 6 * hour

timeLeftForNewYear : Time.Zone -> Time.Posix -> Time.Posix
timeLeftForNewYear here now =
  Time.millisToPosix ((yearToMillis here now) - Time.posixToMillis (now))

yearToMillis : Time.Zone -> Time.Posix -> Int
yearToMillis here now =
  year * (Time.toYear here now) - year * 1970

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick

-- VIEW
toInt: Time.Month -> Int
toInt month = 
  case month of
    Time.Jan -> 1
    Time.Feb -> 2
    Time.Mar -> 3
    Time.Apr -> 4
    Time.May -> 5
    Time.Jun -> 6
    Time.Jul -> 7
    Time.Aug -> 8
    Time.Sep -> 9
    Time.Oct -> 10
    Time.Nov -> 11
    Time.Dec -> 12

view : Model -> Html Msg
view model =
  div []
  [
    let
      months = String.fromInt (toInt (Time.toMonth model.zone model.timeLeft))
      days = String.fromInt (Time.toDay model.zone model.timeLeft)
      hoursLeft = String.fromInt (Time.toHour model.zone model.timeLeft)
      minutesLeft = String.fromInt (Time.toMinute model.zone model.timeLeft)
      secondsLeft = String.fromInt (Time.toSecond model.zone model.timeLeft)
    in
      h1 [] [ text (months ++ " months, " ++ days ++ " days, " ++ hoursLeft ++ " hours, " ++ minutesLeft ++ " minutes and " ++ secondsLeft ++ " seconds left in the year!") ]
  ]