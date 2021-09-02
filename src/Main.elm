module Main exposing (main)

import Browser
import Dict exposing (update)
import Html exposing (Html, div, text)



-- Main


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- Model


type alias Model =
    Int


init : Model
init =
    0



-- Update


update : msg -> Model -> Model
update _ model =
    model



-- View


view : Model -> Html msg
view _ =
    div [] [ text "Hello world!" ]
