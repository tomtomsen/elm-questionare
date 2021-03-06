module Main exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (update)
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (class, type_, name, checked, style)
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
    | Reset


init : Model
init =
    initModel

initModel : Model
initModel = 
    { questions =
        Array.fromList
            [ { question = "Which two words traditionally appear onscreen at the termination of a feature film?"
              , answers =
                    [ "The End"
                    , "The Conclusion"
                    , "The Finish"
                    , "The Pizza Rolls Are Done"
                    ]
              , correct = "The End"
              }
            , { question = "A magnet would most likely attract which of the following?"
              , answers =
                    [ "Metal"
                    , "Plastic"
                    , "Wood"
                    , "The wrong man"
                    ]
              , correct = "Metal"
              }
            , { question = "Which of these names is not in the title of a Shakespeare play?"
              , answers =
                    [ "Hamlet"
                    , "Romeo"
                    , "Macbath"
                    , "Darren"
                    ]
              , correct = "Darren"
              }
            , { question = "Where did Scotch whisky originate?"
              , answers =
                    [ "Ireland"
                    , "Wales"
                    , "Scotland"
                    , "The United Stated"
                    ]
              , correct = "Scotland"
              }
            , { question = "In which of these films does Whoopi Goldberg dress up as a nun?"
              , answers =
                    [ "Sister Act"
                    , "Ghost"
                    , "The Color Purple"
                    , "How Judas Got His Groove Back"
                    ]
              , correct = "Sister Act"
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

        Reset ->
            updateReset


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

updateReset : Model
updateReset =
    initModel


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
            model.currentQuestion == Array.length model.questions
    in
    if end == True then
        viewScore model

    else
        case maybeQuestion of
            Nothing ->
                viewQuestionNotFound

            Just question ->
                viewContainer <|
                    viewPanel <|
                        viewQuestion
                            question
                            model.currentSelected


viewQuestionNotFound : Html Msg
viewQuestionNotFound =
    let
        hint = "Question not found"
    in
    viewContainer <|
        viewPanel <|
            div []
            [ div
                [ class "panel--title"
                ]
                [ text "That was unexpected" ]
            , div
                [ class "panel--content"
                , style "text-align" "center" ]
                [ text hint ]
            , button
                [ onClick Reset
                , class "panel--button"
                ]
                [ text "Restart" ]
            ]


viewScore : Model -> Html Msg
viewScore model =
    let
        hint = (String.fromInt model.correctAnswers) 
            ++ " of " 
            ++ String.fromInt (Array.length model.questions)
            ++ " answers correct"
    in
    viewContainer <|
        viewPanel <|
            div []
            [ div
                [ class "panel--title"
                ]
                [ text "Questionare complete" ]
            , div
                [ class "panel--content"
                , style "text-align" "center" ]
                [ text hint ]
            , button
                [ onClick Reset
                , class "panel--button"
                ]
                [ text "Restart" ]
            ]



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


viewQuestion : Question -> Maybe Answer -> Html Msg
viewQuestion question maybeSelectedAnswer =
    div []
        [ div
            [ class "panel--title"
            ]
            [ text question.question ]
        , div
            [ class "panel--content" ]
            [ viewAnswers
                question.answers
                maybeSelectedAnswer
            ]
        , button
            [ onClick Submit
            , class "panel--button"
            ]
            [ text "Submit" ]
        ]


viewAnswers : Answers -> Maybe Answer -> Html Msg
viewAnswers answers maybeSelected =
    let
        divAnswers =
            List.map
                (\x ->
                    viewAnswer x maybeSelected
                )
                answers
    in
    div [] divAnswers


viewAnswer : Answer -> Maybe Answer -> Html Msg
viewAnswer answer maybeSelected =
    div
        [ onClick (Select answer)
        , class "answer"
        ]
        [ 
            label [] [
                input [ type_ "radio"
                , name "answer"
                , checked (Just answer == maybeSelected)
                ] []
                , text answer
            ]
        ]
