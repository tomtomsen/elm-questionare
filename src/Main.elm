module Main exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (update)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)



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
                div []
                    [ viewContainer <|
                        viewPanel <|
                            viewQuestion
                                question
                    , viewSelectedAnswer
                        model.currentSelected
                    ]


viewQuestionNotFound : Html Msg
viewQuestionNotFound =
    div [] [ text "Question not found" ]


viewScore : Html Msg
viewScore =
    div [] [ text "score" ]


viewContainer : Html Msg -> Html Msg
viewContainer body =
    div
        []
        [ body ]


viewPanel : Html Msg -> Html Msg
viewPanel body =
    div
        [ class "panel"
        ]
        [ body ]


viewQuestion : Question -> Html Msg
viewQuestion question =
    div []
        [ div
            [ class "panel--title"
            ]
            [ text question.question ]
        , div
            [ class "panel--content" ]
            [ viewAnswers
                question.answers
            ]
        , button
            [ onClick Submit ]
            [ text "Submit" ]
        ]


viewAnswers : Answers -> Html Msg
viewAnswers answers =
    let
        divAnswers =
            List.map viewAnswer answers
    in
    div [] divAnswers


viewSelectedAnswer : Maybe Answer -> Html Msg
viewSelectedAnswer answer =
    div []
        [ text ("Auswahl: " ++ Maybe.withDefault "" answer) ]


viewAnswer : Answer -> Html Msg
viewAnswer answer =
    div
        [ onClick (Select answer)
        ]
        [ text ("Answer: " ++ answer) ]
