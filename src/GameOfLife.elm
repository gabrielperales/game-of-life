module GameOfLife exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (..)
import Array exposing (Array)
import List
import Random


type alias BoardShape =
    ( Int, Int )


type alias CellCoords =
    ( Int, Int )


type alias Board =
    Array Bool



-- MODEL


type alias Model =
    { shape : BoardShape
    , cells : Board
    }


empty : BoardShape -> Model
empty shape =
    let
        boardLenght =
            (Tuple.first shape) * (Tuple.second shape)

        cells =
            Array.repeat boardLenght False
    in
        { cells = cells
        , shape = shape
        }


indexToCoords : BoardShape -> Int -> CellCoords
indexToCoords shape index =
    let
        width =
            shape |> Tuple.first

        y =
            index // width

        x =
            index % width
    in
        ( x, y )


coordsToIndex : BoardShape -> CellCoords -> Int
coordsToIndex shape coords =
    let
        width =
            shape |> Tuple.first

        x =
            coords |> Tuple.first

        y =
            coords |> Tuple.second
    in
        y * width + x



-- We need to get the neighbors of a given cellIndex
-- to do that, and because we store our cells inside a list,
-- we need to know the width of each row


neighbors : Model -> Int -> Int
neighbors board cellIndex =
    let
        boardShape : BoardShape
        boardShape =
            .shape board

        cellCoords : CellCoords
        cellCoords =
            indexToCoords boardShape cellIndex
    in
        [ ( -1, -1 )
        , ( 0, -1 )
        , ( 1, -1 )
        , ( 0, -1 )
        , ( 0, 1 )
        , ( 1, -1 )
        , ( 1, 0 )
        , ( 1, 1 )
        ]
            |> List.map (\t -> ( (Tuple.first t) + (Tuple.first cellCoords), (Tuple.second t) + (Tuple.second cellCoords) ))
            |> List.map (coordsToIndex (.shape board))
            |> List.filterMap ((flip Array.get) (.cells board))
            |> List.filter ((==) True)
            |> List.length


isAlive : Bool -> Int -> Bool
isAlive alive neighbors =
    alive && neighbors == 2 || neighbors == 3



-- UPDATE


type Msg
    = Update
    | Init Board


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Update ->
            ( lifeCycle model, Cmd.none )

        Init cells ->
            ( { model | cells = cells }, Cmd.none )


lifeCycle : Model -> Model
lifeCycle board =
    let
        boardShape =
            .shape board

        boardCells =
            .cells board
                |> Array.indexedMap (\index cell -> isAlive cell (neighbors board index))
    in
        { shape = boardShape
        , cells = boardCells
        }



-- VIEW


view : Model -> Html Msg
view model =
    let
        width =
            Tuple.second (.shape model)

        cells =
            .cells model

        split : Int -> List a -> List (List a)
        split i list =
            case List.take i list of
                [] ->
                    []

                listHead ->
                    listHead :: split i (List.drop i list)
    in
        cells
            |> Array.toList
            |> List.map (\c -> [ input [ type_ "checkbox", checked c ] [] ])
            |> List.map (td [])
            |> split width
            |> List.map (tr [])
            |> table []



-- SUBCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second (\_ -> Update)


init : ( Model, Cmd Msg )
init =
    ( empty ( 20, 20 ), Random.generate Init (Random.map (Array.fromList) (Random.list 400 Random.bool)) )


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }
