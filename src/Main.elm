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


type AnswerId
    = AnswerId Int


type alias Model =
    { currentQuestion : Int
    , correctAnswers : Int
    , currentSelected : Maybe AnswerId
    }


init : Model
init =
    { currentQuestion = 0
    , correctAnswers = 0
    , currentSelected = Nothing
    }



-- Update


update : msg -> Model -> Model
update _ model =
    model



-- View


view : Model -> Html msg
view _ =
    div [] [ text "Hello world!" ]
