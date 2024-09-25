module Board exposing
    ( Board
    , Field
    , FieldContent(..)
    , Visibility(..)
    , calcSurrounding
    , clickedField
    )

import Array exposing (Array)


type alias Board =
    { fields : Array Field
    , width : Int
    , height : Int
    }


type alias Field =
    { visibility : Visibility
    , content : FieldContent
    }


type FieldContent
    = Empty Int
    | Bomb


type Visibility
    = Visible
    | Hidden


clickedField : Int -> Board -> Board
clickedField idx board =
    case Array.get idx board.fields of
        Just { content, visibility } ->
            if visibility == Visible then
                board

            else
                case content of
                    Bomb ->
                        revealAll board

                    Empty 0 ->
                        let
                            ( tx, ty ) =
                                toCoords board.width idx

                            withCurrentRevealed =
                                reveal idx board
                        in
                        [ ( -1, 0 ), ( 0, -1 ), ( 1, 0 ), ( 0, 1 ) ]
                            |> List.map (\( dx, dy ) -> ( tx + dx, ty + dy ))
                            |> List.filter (inBounds board.width board.height)
                            |> List.map (toIdx board.width)
                            |> List.foldl clickedField withCurrentRevealed

                    _ ->
                        reveal idx board

        Nothing ->
            board


revealAll : Board -> Board
revealAll board =
    { board | fields = Array.map (\f -> { f | visibility = Visible }) board.fields }


toCoords : Int -> Int -> ( Int, Int )
toCoords width idx =
    ( modBy width idx, idx // width )


toIdx : Int -> ( Int, Int ) -> Int
toIdx width ( x, y ) =
    y * width + x


calcSurrounding : Int -> Int -> List Field -> Board
calcSurrounding width height fields =
    let
        fieldsWithSurrunding =
            List.indexedMap (surroundingBombs width height fields) fields |> Array.fromList
    in
    { fields = fieldsWithSurrunding, width = width, height = height }


inBounds : Int -> Int -> ( Int, Int ) -> Bool
inBounds width height ( x, y ) =
    x >= 0 && y >= 0 && x < width && y < height


surroundingBombs : Int -> Int -> List Field -> Int -> Field -> Field
surroundingBombs width height fields targetIdx field =
    case ( field.visibility, field.content ) of
        ( _, Bomb ) ->
            field

        ( _, Empty _ ) ->
            let
                ( tx, ty ) =
                    toCoords width targetIdx

                isBomb coords =
                    (List.drop (toIdx width coords) fields |> List.head) == Just { visibility = Hidden, content = Bomb }

                numBombs =
                    [ ( -1, -1 ), ( 0, -1 ), ( 1, -1 ), ( -1, 0 ), ( 1, 0 ), ( -1, 1 ), ( 0, 1 ), ( 1, 1 ) ]
                        |> List.map (\( x, y ) -> ( tx + x, ty + y ))
                        |> List.filter (inBounds width height)
                        |> List.filter isBomb
                        |> List.length
            in
            { field | content = Empty numBombs }


reveal : Int -> Board -> Board
reveal target board =
    case Array.get target board.fields of
        Just field ->
            { board | fields = Array.set target { field | visibility = Visible } board.fields }

        Nothing ->
            board
