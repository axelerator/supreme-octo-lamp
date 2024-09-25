module Main exposing (main)

import Array
import Board exposing (Board, Field, FieldContent(..), Visibility(..), calcSurrounding, clickedField)
import Browser
import Html exposing (button, div, main_, text)
import Html.Attributes exposing (attribute, class, id)
import Html.Events exposing (onClick)
import Random exposing (generate)
import Random.List exposing (shuffle)


type alias Model =
    { board : Board
    , mode : Mode
    }


type Mode
    = InGame
    | InMenu


main : Program ( Int, Int, Int ) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : ( Int, Int, Int ) -> ( Model, Cmd Msg )
init ( numBombs, width, height ) =
    let
        unshuffled =
            [ List.repeat numBombs { visibility = Hidden, content = Bomb }
            , List.repeat (width * height - numBombs) { visibility = Hidden, content = Empty -1 }
            ]
                |> List.concat
    in
    ( { board =
            { fields = Array.empty, width = width, height = height }
      , mode = InMenu
      }
    , generate (GotShuffledFields width height) (shuffle unshuffled)
    )


type Msg
    = ClickedField Int
    | GotShuffledFields Int Int (List { visibility : Visibility, content : FieldContent })
    | ClickedStart
    | ClickedBackToMenu


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ClickedField idx ->
            ( { model | board = clickedField idx model.board }
            , Cmd.none
            )

        GotShuffledFields width height fields ->
            ( { model | board = calcSurrounding width height fields }
            , Cmd.none
            )

        ClickedStart ->
            ( { model | mode = InGame }
            , Cmd.none
            )

        ClickedBackToMenu ->
            ( { model | mode = InMenu }
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
        [ viewMenu
        , viewBoard model
        ]


viewMenu : Html.Html Msg
viewMenu =
    div [ id "menu" ] [ text "menu", button [ onClick ClickedStart ] [ text "start" ] ]


viewBoard : Model -> Html.Html Msg
viewBoard { board } =
    let
        { fields, width } =
            board

        numHidden =
            Array.filter (\{ visibility } -> visibility == Hidden) fields |> Array.length

        numBombs =
            Array.filter (\{ content } -> content == Bomb) fields |> Array.length
    in
    div [ id "board-container" ]
        [ div [ id "board", attribute "style" ("--width: " ++ String.fromInt width) ] <| Array.toList <| Array.indexedMap viewField fields
        , if numBombs == numHidden then
            text "you won"

          else
            text ""
        , button [ onClick ClickedBackToMenu ] [ text "back" ]
        ]


viewField : Int -> Field -> Html.Html Msg
viewField idx { visibility, content } =
    case ( visibility, content ) of
        ( Visible, Empty 0 ) ->
            div [] []

        ( Visible, Empty n ) ->
            div [] [ text (String.fromInt n) ]

        ( Visible, Bomb ) ->
            div [] [ text "💣" ]

        _ ->
            div [ onClick (ClickedField idx), class "hidden" ] []
