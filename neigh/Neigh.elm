import Char
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import String
import Task exposing (..)


-- VIEW

view : String -> Result String (List String) -> Html
view string result =
  let field =
        input
          [ placeholder "Zip Code"
          , value string
          , on "input" targetValue (Signal.message query.address)
          , myStyle
          ]
          []

      messages =
        case result of
          Err msg ->
              [ div [ myStyle ] [ text msg ] ]

          Ok cities ->
              List.map (\city -> div [ myStyle ] [ text city ]) cities
  in
      div [] (field :: messages)


myStyle : Attribute
myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]


-- WIRING

main =
  Signal.map2 view query.signal results.signal


query : Signal.Mailbox String
query =
  Signal.mailbox ""


results : Signal.Mailbox (Result String (List String))
results =
  Signal.mailbox (Err "A valid US zip code is 5 numbers.")


port requests : Signal (Task x ())
port requests =
  Signal.map postPonyMsg query.signal
    |> Signal.map (\task -> Task.toResult task `andThen` Signal.send results.address)


ponyurl = "https://slack.com/api/chat.postMessage?token=xoxp-16403402883-16552290308-23955244178-3e1b136b9e&pretty=1&channel=%23luktnypon&username=TreeHorse&text="

postPonyMsg : String -> Task String (List String)
postPonyMsg query =
  let toUrl =
        if String.length query > 5
          then succeed (ponyurl ++ query)
          else fail "Give me some message!"
  in
      toUrl `andThen` (mapError (always "Not ok :(") << Http.get poststatus)


poststatus : Json.Decoder (List String)
poststatus = 
  let channel = 
    "channel" := Json.string
  in
     "places" := Json.list channel
