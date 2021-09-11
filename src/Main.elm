module Main exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (update)
import Element exposing (Element, column, el, rgba255, text)
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Html exposing (Html)



-- Main


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- Model


type alias Answer =
    String


type alias Answers =
    List Answer


type alias Question =
    { question : String
    , answers : Answers
    , correct : Answer
    }


type alias Questions =
    Array Question


type alias Model =
    { questions : Questions
    , currentQuestion : Int
    , correctAnswers : Int
    , currentSelected : Maybe Answer
    }


type Msg
    = Select Answer
    | Submit


init : Model
init =
    { questions =
        Array.fromList
            [ { question = "First question"
              , answers =
                    [ "Test"
                    , "Other"
                    ]
              , correct = "Test"
              }
            , { question = "Second question"
              , answers =
                    [ "Yes"
                    , "No"
                    ]
              , correct = "Yes"
              }
            ]
    , currentQuestion = 0
    , correctAnswers = 0
    , currentSelected = Nothing
    }



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        Select answerId ->
            updateSelect answerId model

        Submit ->
            updateSubmit model


updateSelect : Answer -> Model -> Model
updateSelect answer model =
    { model | currentSelected = Just answer }


updateSubmit : Model -> Model
updateSubmit model =
    let
        correct =
            isAnswerCorrect model
    in
    if model.currentSelected == Nothing then
        model

    else if correct then
        { model
            | currentQuestion = model.currentQuestion + 1
            , currentSelected = Nothing
            , correctAnswers = model.correctAnswers + 1
        }

    else
        { model
            | currentQuestion = model.currentQuestion + 1
            , currentSelected = Nothing
        }


isAnswerCorrect : Model -> Bool
isAnswerCorrect model =
    let
        maybeQuestion =
            Array.get model.currentQuestion model.questions

        maybeSelected =
            model.currentSelected
    in
    case maybeQuestion of
        Nothing ->
            False

        Just question ->
            case maybeSelected of
                Nothing ->
                    False

                Just selected ->
                    selected == question.correct



-- View


view : Model -> Html Msg
view model =
    Element.layout
        [ Font.size 18
        , Font.family
            [ Font.external
                { name = "Roboto"
                , url = "https://fonts.googleapis.com/css?family=Roboto"
                }
            , Font.sansSerif
            ]
        ]
    <|
        viewLayout model


viewLayout : Model -> Element Msg
viewLayout model =
    let
        maybeQuestion =
            Array.get model.currentQuestion model.questions

        end =
            model.currentQuestion >= Array.length model.questions
    in
    if end == True then
        viewScore

    else
        case maybeQuestion of
            Nothing ->
                viewQuestionNotFound

            Just question ->
                viewContainer <|
                    viewPanel <|
                        column []
                            [ viewQuestion
                                question
                            , viewSelectedAnswer
                                model.currentSelected
                            ]


viewQuestionNotFound : Element Msg
viewQuestionNotFound =
    el [] <| text "Question not found"


viewScore : Element Msg
viewScore =
    el [] <| text "score"


viewContainer : Element Msg -> Element Msg
viewContainer body =
    el
        [ Element.centerX
        , Element.centerY
        ]
        body


viewPanel : Element Msg -> Element Msg
viewPanel body =
    el
        [ Border.width 1
        , Border.rounded 5
        , Border.glow (rgba255 65 72 86 0.25) 5
        , Element.padding 5
        ]
        body


viewQuestion : Question -> Element Msg
viewQuestion question =
    column []
        [ el [] <|
            text
                question.question
        , el [] <|
            viewAnswers
                question.answers
        , viewButton Submit
        ]


viewButton : Msg -> Element Msg
viewButton msg =
    el [ onClick msg ] <|
        text
            "Submit"


viewAnswers : Answers -> Element Msg
viewAnswers answers =
    let
        divAnswers =
            List.map viewAnswer answers
    in
    column [] divAnswers


viewSelectedAnswer : Maybe Answer -> Element Msg
viewSelectedAnswer answer =
    el [] <| text ("Auswahl: " ++ Maybe.withDefault "" answer)


viewAnswer : Answer -> Element Msg
viewAnswer answer =
    el
        [ onClick (Select answer)
        ]
    <|
        text
            ("Answer: " ++ answer)
