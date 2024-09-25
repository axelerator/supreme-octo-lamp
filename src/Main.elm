module Main exposing (main)

import Browser
import Html exposing (div, text)
import Html.Attributes exposing (attribute, class, id)
import Html.Events exposing (onClick)
import Random exposing (generate)
import Random.List exposing (shuffle)
import Board exposing (FieldContent, Board)
import Board exposing (Visibility(..))
import Board exposing (FieldContent(..))
import Array
import Board exposing (clickedField)
import Board exposing (calcSurrounding)
import Board exposing (Field)


main : Program ( Int, Int, Int ) Board Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : ( Int, Int, Int ) -> ( Board, Cmd Msg )
init ( numBombs, width, height ) =
    let
        unshuffled =
            [ List.repeat numBombs { visibility = Hidden, content = Bomb }
            , List.repeat (width * height - numBombs) { visibility = Hidden, content = Empty -1 }
            ]
                |> List.concat
    in
    ( { fields = Array.empty, width = width, height = height }
    , generate (GotShuffledFields width height) (shuffle unshuffled)
    )


type Msg
    = ClickedField Int
    | GotShuffledFields Int Int (List { visibility : Visibility, content : FieldContent })


update : Msg -> Board -> ( Board, Cmd msg )
update msg board =
    case msg of
        ClickedField idx ->
            ( clickedField idx board, Cmd.none )

        GotShuffledFields width height fields ->
            ( calcSurrounding width height fields, Cmd.none )



view : Board -> Html.Html Msg
view { fields, width } =
    let
        numHidden =
            Array.filter (\{ visibility } -> visibility == Hidden) fields |> Array.length

        numBombs =
            Array.filter (\{ content } -> content == Bomb) fields |> Array.length
    in
    div []
        [ div [ id "board", attribute "style" ("--width: " ++ String.fromInt width) ] <| Array.toList <| Array.indexedMap viewField fields
        , if numBombs == numHidden then
            text "you won"

          else
            text ""
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
