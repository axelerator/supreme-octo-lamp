module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (id, attribute, class)
import Random exposing (generate)
import Random.List exposing (shuffle)
import Array

main = Browser.element { init = init , update = update, view = view, subscriptions = \_ -> Sub.none }

init (numBombs, width, height) =
  let
      unshuffled =
          [ List.repeat numBombs { visibility = Hidden, content = Bomb} 
          , List.repeat (width * height - numBombs) { visibility = Hidden, content = Empty -1 }
          ] |> List.concat
  in
  ( { fields = Array.empty, width = width, height = height }
  , generate (GotShuffledFields width height) (shuffle unshuffled)
  )

type Msg = ClickedField Int | GotShuffledFields Int Int (List { visibility: Visibility, content: FieldContent })

type FieldContent = Empty Int | Bomb
type Visibility = Visible | Hidden

update msg board =
  case msg of
    ClickedField idx -> (clickedField idx board , Cmd.none)
    GotShuffledFields width height fields -> (calcSurrounding width height fields, Cmd.none)

clickedField idx board = 
  case Array.get idx board.fields of
    Just { content, visibility } ->
      if visibility == Visible then
        board
      else
        case content of
          Bomb -> revealAll board
          Empty 0 -> 
            let
                (tx, ty) = toCoords board.width idx
                withCurrentRevealed = reveal idx board
            in
                [(-1, 0), (0, -1), (1, 0), (0, 1)]      
                |> List.map (\(dx, dy) -> (tx + dx, ty + dy))
                |> List.filter (inBounds board.width board.height)
                |> List.map (toIdx board.width)
                |> List.foldl clickedField withCurrentRevealed 

          _ -> reveal idx board
    Nothing -> board

revealAll board = { board | fields =  Array.map (\f -> { f | visibility = Visible }) board.fields }

toCoords width idx = (modBy width idx, idx // width)
toIdx width (x,y) = ( y * width + x )

calcSurrounding width height fields = 
  let
      bombLocations = List.indexedMap (\i f -> (i, f)) fields
        |> List.filter (\(_, {content}) -> content == Bomb) 
        |> List.map Tuple.first
        |> List.map toCoords
      fieldsWithSurrunding = List.indexedMap (surroundingBombs bombLocations width height fields) fields |> Array.fromList
  in
    { fields = fieldsWithSurrunding, width = width, height = height }

inBounds width height (x,y) = x >= 0 && y >= 0 && x < width && y < height

surroundingBombs bombLocations width height fields targetIdx field =
  case (field.visibility,field.content) of
    (_, Bomb) -> field
    (_, Empty _) ->
      let
          (tx, ty) = toCoords width targetIdx
          isBomb coords =
              (List.drop (toIdx width coords) fields |> List.head) == Just { visibility = Hidden, content = Bomb }
          numBombs =
            [(-1, -1), (0, -1), (1, -1) , (-1, 0),(1, 0) , (-1, 1), (0, 1), (1, 1)]
            |> List.map (\(x,y) -> (tx + x, ty + y))
            |> List.filter (inBounds width height)
            |> List.filter isBomb
            |> List.length
      in
      { field | content = Empty numBombs }
    
reveal target board =
  case Array.get target board.fields of
      Just field -> { board | fields = Array.set target { field | visibility = Visible } board.fields }
      Nothing -> board

view {fields, width} = 
  let
      numHidden = Array.filter (\{visibility} -> visibility == Hidden) fields |> Array.length
      numBombs = Array.filter (\{content} -> content == Bomb) fields |> Array.length
  in
  div []
   [ div [id "board", attribute "style" ("--width: " ++ String.fromInt width)] <| Array.toList <| (Array.indexedMap viewField fields) 
   , if numBombs == numHidden then text "you won" else text ""
   ]

viewField idx {visibility, content} =
  case (visibility, content) of
    (Visible, Empty 0) -> div [] []
    (Visible, Empty n) -> div [] [text (String.fromInt n)]
    (Visible, Bomb) -> div [] [text "💣"]
    _ -> div [onClick (ClickedField idx), class "hidden"] []
