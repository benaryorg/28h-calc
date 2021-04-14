module Main exposing (..)

import Browser
import Array
import Html exposing (Html, input, div, span, text, h1, select, option, button, a)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (style, type_, checked, value, selected, href)


-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

days : Array.Array String
days = Array.fromList ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
totalhours : Int
totalhours = 6*28

type alias Row =
  { day : String
  , indices : List Int
  }

type Action
  = Toggle
  | Set
  | Clear

type alias Hours = Array.Array Bool

mapIntervalAction: (Action -> Action) -> Interval -> Interval
mapIntervalAction map interval = { interval | action = map interval.action }
mapIntervalEvery: (Int -> Int) -> Interval -> Interval
mapIntervalEvery map interval = { interval | every = map interval.every }
mapIntervalLength: (Int -> Int) -> Interval -> Interval
mapIntervalLength map interval = { interval | length = map interval.length }
mapIntervalOffset: (Int -> Int) -> Interval -> Interval
mapIntervalOffset map interval = { interval | offset = map interval.offset }

type alias Interval =
  { action: Action
  , every: Int
  , length: Int
  , offset: Int
  }

mapHours: (Hours -> Hours) -> Model -> Model
mapHours map model = { model | hours = map model.hours }
mapOffset: (Int -> Int) -> Model -> Model
mapOffset map model = { model | offset = map model.offset }
mapInterval: (Interval -> Interval) -> Model -> Model
mapInterval map model = { model | interval = map model.interval }

type alias Model =
  { hours: Hours
  , offset: Int
  , interval: Interval
  }


init : Model
init =
  { hours = Array.repeat (28*6) False
  , offset = 0
  , interval =
    { action = Toggle
    , every = 28
    , length = 1
    , offset = 0
    }
  }



-- UPDATE


type Msg
  = ToggleHour Int
  | OffsetSelect String
  | IntervalActionSelect String
  | IntervalEverySelect String
  | IntervalLengthSelect String
  | IntervalOffsetSelect String
  | RunInterval


update : Msg -> Model -> Model
update msg =
  case msg of
    ToggleHour idx -> mapHours
      (\hours -> Array.get idx hours
          |> Maybe.withDefault False
          |> not
          |> \value -> Array.set idx value hours
      )
    OffsetSelect s -> case String.toInt s of
      Just n -> (mapOffset <| always n)
      Nothing -> identity
    IntervalActionSelect s -> case s of
      "toggle" -> (mapInterval <| mapIntervalAction <| always Toggle)
      "set" -> (mapInterval <| mapIntervalAction <| always Set)
      "clear" -> (mapInterval <| mapIntervalAction <| always Clear)
      _ -> identity
    IntervalEverySelect s -> case String.toInt s of
      Just n -> (mapInterval <| mapIntervalEvery <| always n)
      Nothing -> identity
    IntervalLengthSelect s -> case String.toInt s of
      Just n -> (mapInterval <| mapIntervalLength <| always n)
      Nothing -> identity
    IntervalOffsetSelect s -> case String.toInt s of
      Just n -> (mapInterval <| mapIntervalOffset <| always n)
      Nothing -> identity
    RunInterval -> (\model ->
        List.range 1 (totalhours//model.interval.every)
          |> List.map (\x -> (x-1)*model.interval.every+model.interval.offset)
          |> List.map (\x -> List.range x (x+model.interval.length-1))
          |> List.foldr List.append []
          |> List.map (\x -> modBy totalhours x)
          |> List.foldr
              (\idx hours ->
                Array.get idx hours
                  |> Maybe.withDefault False
                  |> (case model.interval.action of
                      Toggle -> not
                      Set -> always True
                      Clear -> always False
                    )
                  |> \value -> Array.set idx value hours
              )
              model.hours
          |> \hours -> { model | hours = hours }
      )

-- VIEW

renderrow : Hours -> Row -> Html Msg
renderrow model row = row.indices
  |> List.map (\idx -> Array.get idx model |> Maybe.withDefault False |> \state -> (idx,state))
  |> List.map (\(idx,state) -> input [ type_ "checkbox", checked state, onClick (ToggleHour idx)] [])
  |> (::) (span [style "font-family" "DejaVu Sans Mono"] [text row.day])
  |> div []

rowfordays : Int -> Int -> List Row
rowfordays numberofdays offset =
  let
    hoursperday = totalhours//numberofdays
  in
    List.range 0 (numberofdays-1)
      |> List.map (\day ->
          { day = Array.get day days |> Maybe.withDefault "unknown"
          , indices = List.range (day*hoursperday) ((day+1)*hoursperday-1) |> List.map (\hour -> modBy totalhours (hour+offset))
          }
        )

intervalview : Interval -> Html Msg
intervalview interval = div []
  [ button [onClick RunInterval] [text "Do it!"]
  , select [onInput IntervalActionSelect]
    [ option [value "toggle"] [text "toggle"]
    , option [value "set"] [text "set"]
    , option [value "clear"] [text "clear"]
    ]
  , [28,24,14,12,8,7,6,4,3,2]
      |> List.map (\n -> option [value (String.fromInt n), selected (interval.every == n)] [text ("every " ++ String.fromInt n ++ "h")])
      |> select [onInput IntervalEverySelect]
  , List.range 1 interval.every
      |> List.map (\n -> option [value (String.fromInt n), selected (interval.length == n)] [text (String.fromInt n ++ "h long")])
      |> select [onInput IntervalLengthSelect]
  , List.range 0 (interval.every-interval.length)
      |> List.map (\n -> option [value (String.fromInt n), selected (interval.offset == n)] [text (String.fromInt n ++ "h offset")])
      |> select [onInput IntervalOffsetSelect]
  ]

view : Model -> Html Msg
view model = div []
  [ h1 [] [text "28h"]
  , List.range 0 27
      |> List.map (\n -> option [value (String.fromInt n)] [text (String.fromInt n ++ "h offset")])
      |> select [onInput OffsetSelect]
      |> List.singleton
      |> div []
  , rowfordays 6 model.offset |> List.map (renderrow model.hours) |> div []
  , h1 [] [text "24h"]
  , rowfordays 7 0 |> List.map (renderrow model.hours) |> div []
  , h1 [] [text "Intervals"]
  , intervalview model.interval
  , div []
    [ text "Created by "
    , span [ style "font-family" "DejaVu Sans Mono" ] [ text "benaryorg" ]
    , text ", licensed under ISC license, see the "
    , a [ href "https://github.com/benaryorg/28h-calc" ] [ text "repository" ]
    , text " for further information."
    ]
  ]

-- vim: set tabstop=2 shiftwidth=2 softtabstop=2 expandtab:

