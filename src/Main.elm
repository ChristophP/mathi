module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Icons
import Process
import Random
import Random.List exposing (choose, shuffle)
import Task
import Util exposing (listCount)


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL AND TYPES


type alias Model =
    { page : Page
    , seed : Random.Seed
    }


type Page
    = Start
    | Play PlayState
    | Gameover (List ( Problem, Int )) Bool


updatePlay : (PlayState -> PlayState) -> Page -> Page
updatePlay fn page =
    case page of
        Play playState ->
            Play (fn playState)

        _ ->
            page


addNewAnswer : Int -> PlayState -> PlayState
addNewAnswer answer playState =
    let
        previousProblems =
            playState.previousProblems
                ++ [ ( playState.currentProblem, answer ) ]
    in
    { playState
        | currentAnswer = Just answer
        , previousProblems = previousProblems
    }


type Problem
    = Problem Int Op Int


getProblemAnswer : Problem -> Int
getProblemAnswer (Problem num1 Plus num2) =
    num1 + num2


isCorrectAnswer : ( Problem, Int ) -> Bool
isCorrectAnswer ( problem, int ) =
    getProblemAnswer problem == int


type Op
    = Plus


type alias PlayState =
    { currentProblem : Problem
    , previousProblems : List ( Problem, Int )
    , currentAnswer : Maybe Int
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { page = Start, seed = Random.initialSeed 0 }
    , Random.generate GotSeed Random.independentSeed
    )



-- CONSTANTS AND RANDOM GENERATORS


maxNum : Int
maxNum =
    6


maxQuestions : Int
maxQuestions =
    5


listWithoutNum : Int -> List Int
listWithoutNum num =
    List.range 0 (num - 1)
        ++ List.range (num + 1) (maxNum * 2)


wrongAnswersGenerator : Int -> Random.Generator ( Int, Int )
wrongAnswersGenerator correctAnswer =
    let
        wrongAnswers =
            listWithoutNum correctAnswer
    in
    choose wrongAnswers
        |> Random.andThen
            (\( maybeAnswer1, rest ) ->
                choose rest
                    |> Random.map
                        (\( maybeAnswer2, _ ) ->
                            Tuple.mapBoth
                                (Maybe.withDefault 0)
                                (Maybe.withDefault 0)
                                ( maybeAnswer1, maybeAnswer2 )
                        )
            )


numberGenerator : Random.Generator ( Int, Int )
numberGenerator =
    Random.pair (Random.int 0 maxNum) (Random.int 0 maxNum)


fruitGenerator : Random.Generator String
fruitGenerator =
    Random.uniform "🍓" [ "🍏", "🍉", "🍋" ]



-- UPDATE


type Msg
    = GotSeed Random.Seed
    | StartGame
    | StepGame
    | Answer Int
    | GameOverLoaded


stepGame : Model -> ( Model, Cmd Msg )
stepGame model =
    case model.page of
        Play { previousProblems } ->
            if List.length previousProblems >= maxQuestions then
                ( { model | page = Gameover previousProblems False }
                , Task.perform (\_ -> GameOverLoaded) (Process.sleep 100)
                )

            else
                let
                    ( ( firstNum, secondNum ), newSeed ) =
                        Random.step numberGenerator model.seed

                    setPlayState playState =
                        { playState
                            | currentProblem = Problem firstNum Plus secondNum
                            , currentAnswer = Nothing
                        }
                in
                ( { model
                    | page = updatePlay setPlayState model.page
                    , seed = newSeed
                  }
                , Cmd.none
                )

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSeed seed ->
            ( { model | seed = seed }, Cmd.none )

        StartGame ->
            let
                ( ( firstNum, secondNum ), newSeed ) =
                    Random.step numberGenerator model.seed
            in
            ( { model
                | page =
                    Play
                        { currentProblem = Problem firstNum Plus secondNum
                        , previousProblems = []
                        , currentAnswer = Nothing
                        }
                , seed = newSeed
              }
            , Cmd.none
            )

        StepGame ->
            stepGame model

        Answer answer ->
            case model.page of
                Play _ ->
                    let
                        newPage =
                            updatePlay (addNewAnswer answer) model.page
                    in
                    ( { model | page = newPage }
                    , Task.perform (\_ -> StepGame) (Process.sleep 1500)
                    )

                _ ->
                    ( model, Cmd.none )

        GameOverLoaded ->
            case model.page of
                Gameover results _ ->
                    ( { model | page = Gameover results True }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- VIEW


numberWithFruit : Int -> String -> Html msg
numberWithFruit num fruit =
    div [ class "flex flex-col items-center justify-end" ]
        [ div [ class "text-2xl" ] [ text (String.repeat num fruit) ]
        , div [] [ text (String.fromInt num) ]
        ]


viewProblem : Problem -> Random.Seed -> Html msg
viewProblem (Problem firstNum Plus secondNum) seed =
    let
        ( ( fruit1, fruit2 ), _ ) =
            Random.step (Random.pair fruitGenerator fruitGenerator) seed
    in
    div [ class "flex flex-row text-6xl h-gap" ]
        [ numberWithFruit firstNum fruit1
        , div [ class "self-end" ] [ text "+" ]
        , numberWithFruit secondNum fruit2
        ]


viewAnswer : Int -> Html Msg
viewAnswer answer =
    div
        [ class "flex-1 flex items-center justify-center border-purple-700 py-2"
        , class "text-white text-2xl font-bold cursor-pointer bg-purple-700 hover:bg-purple-500 rounded-sm"
        , onClick (Answer answer)
        ]
        [ text (String.fromInt answer) ]


withPlayFrame : List (Html msg) -> Html msg -> List (Html msg)
withPlayFrame questions results =
    [ main_ [ class "flex flex-row w-full items-center" ]
        [ div [ class "flex flex-col w-full items-center" ] questions
        , div [ class "px-2" ] [ results ]
        ]
    ]


viewStars : List ( Problem, Int ) -> Html msg
viewStars previousProblems =
    let
        stars =
            List.map
                (\item ->
                    if isCorrectAnswer item then
                        span [ class "text-yellow-600" ] [ text "★" ]

                    else
                        span [] [ text "☆" ]
                )
                previousProblems

        dots =
            List.repeat (maxQuestions - List.length stars) (text "•")
    in
    stars
        ++ dots
        |> List.intersperse (br [] [])
        |> div [ class "leading-tight text-xs text-center p-2 border-2 border-yellow-600" ]


viewPlay : PlayState -> Random.Seed -> List (Html Msg)
viewPlay { currentProblem, previousProblems, currentAnswer } seed =
    let
        correctAnswer =
            getProblemAnswer currentProblem
    in
    case currentAnswer of
        Nothing ->
            let
                ( ( wrongAnswer1, wrongAnswer2 ), newSeed ) =
                    Random.step (wrongAnswersGenerator correctAnswer) seed

                ( allAnswers, _ ) =
                    Random.step (shuffle [ correctAnswer, wrongAnswer1, wrongAnswer2 ]) newSeed
            in
            withPlayFrame
                [ viewProblem currentProblem seed
                , div [ class "w-full flex flex-row h-gap" ]
                    (List.map viewAnswer allAnswers)
                ]
                (viewStars previousProblems)

        Just answer ->
            withPlayFrame
                [ span [ class "text-4xl font-serif pop" ]
                    [ text <|
                        if answer == getProblemAnswer currentProblem then
                            "😊"

                        else
                            "😢"
                    ]
                ]
                (viewStars previousProblems)


view : Model -> Browser.Document Msg
view model =
    Browser.Document "Mathi!" <|
        [ div [ class "flex flex-col justify-center items-center w-screen h-screen" ] <|
            case model.page of
                Start ->
                    [ h1 [ class "text-4xl text-center" ] [ text "Mathi !" ]
                    , main_ [ class "w-full" ]
                        [ button
                            [ class "bg-yellow-500 p-2 m-auto block w-1/2 md:max-w-xs"
                            , onClick StartGame
                            ]
                            [ text "Los 🡪" ]
                        ]
                    ]

                Play playState ->
                    viewPlay playState model.seed

                Gameover results loaded ->
                    let
                        numRightAnswers =
                            listCount isCorrectAnswer results

                        percentage =
                            if not loaded then
                                0

                            else
                                round (toFloat numRightAnswers / toFloat maxQuestions * 100)
                    in
                    [ div [ class "v-gap text-center" ]
                        [ h2 [] [ text "Game Over" ]
                        , text (String.fromInt numRightAnswers ++ " right answers!")
                        , div [class "w-64"] [Icons.pie percentage]
                        , button
                            [ class "bg-yellow-500 p-2 m-auto block"
                            , onClick StartGame
                            ]
                            [ text "Play Again!" ]
                        ]
                    ]
        ]
