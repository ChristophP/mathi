module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Random
import Random.List exposing (choose, shuffle)


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { page : Page
    , seed : Random.Seed
    }


type Page
    = Start
    | Play PlayState
    | Gameover (List Problem)


type Problem
    = Problem Int Op Int


type Op
    = Plus


type alias PlayState =
    { currentProblem : Problem
    , previousProblems : List Problem
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { page = Start, seed = Random.initialSeed 0 }, Random.generate GotSeed Random.independentSeed )


type Msg
    = GotSeed Random.Seed
    | StartGame
    | StepGame


maxNum =
    4


listWithoutNum num =
    List.range 0 (num - 1)
        ++ List.range (num + 1) maxNum


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
    choose [ "🍓", "🍏", "🍉", "🍋" ]
        |> Random.map (Tuple.first >> Maybe.withDefault "Will-never-happen")


update msg model =
    case msg of
        GotSeed seed ->
            ( { model | seed = seed }, Cmd.none )

        StartGame ->
            let
                ( ( firstNum, secondNum ), newSeed ) =
                    Random.step numberGenerator model.seed
            in
            ( { model | page = Play { currentProblem = Problem firstNum Plus secondNum, previousProblems = [] }, seed = newSeed }
            , Cmd.none
            )

        StepGame ->
            let
                ( ( firstNum, secondNum ), newSeed ) =
                    Random.step numberGenerator model.seed
            in
            ( { model | page = Play { currentProblem = Problem firstNum Plus secondNum, previousProblems = [] }, seed = newSeed }
            , Cmd.none
            )


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


viewAnswer answer =
    div
        [ class "border-4 rounded-2 border-purple-700 cursor-pointer w-10 h-10 flex items-center justify-center"
        , onClick StepGame
        ]
        [ text (String.fromInt answer) ]


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

                Play { currentProblem, previousProblems } ->
                    let
                        (Problem firstNum Plus secondNum) =
                            currentProblem

                        correctAnswer =
                            firstNum + secondNum

                        ( ( wrongAnswer1, wrongAnswer2 ), newSeed ) =
                            Random.step (wrongAnswersGenerator correctAnswer) model.seed

                        ( allAnswers, _ ) =
                            Random.step (shuffle [ correctAnswer, wrongAnswer1, wrongAnswer2 ]) newSeed
                    in
                    [ main_ [ class "flex flex-col w-full items-center" ]
                        [ viewProblem currentProblem model.seed
                        , div [ class "flex flex-row h-gap" ]
                            (List.map viewAnswer allAnswers)
                        ]
                    ]

                Gameover results ->
                    [ text "TODO" ]
        ]
