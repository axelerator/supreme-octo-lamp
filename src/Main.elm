module Main exposing (main)

import Array
import Board exposing (Board, Field, FieldContent(..), Visibility(..), calcSurrounding, clickedField)
import Browser
import Html exposing (button, div, h3, input, main_, text)
import Html.Attributes exposing (attribute, class, id, type_, value)
import Html.Events exposing (onClick, onInput)
import Random exposing (generate)
import Random.List exposing (shuffle)


type alias Model =
    { board : Board
    , mode : Mode
    , size : Int
    , bombMult : Int
    }


type Mode
    = InGame
    | InMenu


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board =
            { fields = Array.empty, width = 1, height = 1 }
      , mode = InMenu
      , size = 2
      , bombMult = 1
      }
    , Cmd.none
    )


type Msg
    = ClickedField Int
    | GotShuffledFields Int Int (List { visibility : Visibility, content : FieldContent })
    | ClickedStart
    | ClickedBackToMenu
    | ChangedSize String
    | ChangedNumBombs String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedField idx ->
            ( { model | board = clickedField idx model.board }
            , Cmd.none
            )

        GotShuffledFields width height fields ->
            ( { model
                | board = calcSurrounding width height fields
                , mode = InGame
              }
            , Cmd.none
            )

        ClickedStart ->
            let
                width =
                    boardWidth model

                numBombs =
                    model.bombMult * width

                unshuffled =
                    [ List.repeat numBombs { visibility = Hidden, content = Bomb }
                    , List.repeat (width * width - numBombs) { visibility = Hidden, content = Empty -1 }
                    ]
                        |> List.concat
            in
            ( model
            , generate (GotShuffledFields width width) (shuffle unshuffled)
            )

        ClickedBackToMenu ->
            ( { model | mode = InMenu }
            , Cmd.none
            )

        ChangedSize sizeStr ->
            let
                size =
                    String.toInt sizeStr |> Maybe.withDefault model.size
            in
            ( { model
                | size = size
                , bombMult = min (2 ^ size) model.bombMult
              }
            , Cmd.none
            )

        ChangedNumBombs mult ->
            ( { model | bombMult = String.toInt mult |> Maybe.withDefault model.bombMult }
            , Cmd.none
            )


view : Model -> Html.Html Msg
view model =
    main_
        [ class <|
            if model.mode == InMenu then
                ""

            else
                "in-game"
        ]
        [ viewMenu model
        , viewBoard model
        ]


boardWidth : Model -> Int
boardWidth model =
    2 ^ model.size


numBombs_ : Model -> Int
numBombs_ model =
    boardWidth model * model.bombMult


viewMenu : Model -> Html.Html Msg
viewMenu model =
    div []
        [ div [ id "menu" ]
            [ h3 [] [ text "Minesweeper" ]
            , div [ class "label" ]
                [ div [] [ text "size:" ]
                , div []
                    [ boardWidth model
                        |> String.fromInt
                        |> text
                    ]
                ]
            , div []
                [ input
                    [ type_ "range"
                    , onInput ChangedSize
                    , Html.Attributes.max "7"
                    , value <| String.fromInt model.size
                    ]
                    []
                ]
            , div [ class "label" ]
                [ div [] [ text "bombs:" ]
                , div []
                    [ numBombs_ model
                        |> String.fromInt
                        |> text
                    ]
                ]
            , div []
                [ input
                    [ type_ "range"
                    , Html.Attributes.max <| String.fromInt <| boardWidth model
                    , value <| String.fromInt <| model.bombMult
                    , onInput ChangedNumBombs
                    ]
                    []
                ]
            , button [ onClick ClickedStart ] [ text "start" ]
            ]
        ]


viewBoard : Model -> Html.Html Msg
viewBoard { board } =
    let
        { fields, width } =
            board

        numHidden =
            Array.filter (\{ visibility } -> visibility == Hidden) fields |> Array.length

        numBombs =
            Array.filter (\{ content } -> content == Bomb) fields |> Array.length

        wonView =
            if numBombs == numHidden then
                div [ id "won" ] [ div [] [ text "you won" ] ]

            else
                text ""
    in
    div [ id "board-container" ]
        [ div
            [ id "board"
            , attribute "style" ("--width: " ++ String.fromInt width)
            ]
          <|
            (::) wonView <|
                Array.toList <|
                    Array.indexedMap viewField fields
        , div [] [ button [ onClick ClickedBackToMenu ] [ text "back" ] ]
        ]


viewField : Int -> Field -> Html.Html Msg
viewField idx { visibility, content } =
    case ( visibility, content ) of
        ( Visible, Empty 0 ) ->
            div [] []

        ( Visible, Empty n ) ->
            div [ attribute "style" <| "--bombs: " ++ String.fromInt n ] [ text (String.fromInt n) ]

        ( Visible, Bomb ) ->
            div [] [ text "💣" ]

        ( Hidden, _ ) ->
            div [ onClick (ClickedField idx), class "hidden" ] []
