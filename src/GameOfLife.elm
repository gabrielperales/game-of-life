module GameOfLife exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Time exposing (..)
import Array exposing (Array)
import List
import Random
import BeautifulExample
import Color


main : Program Never Model Msg
main =
    Html.program
        { view =
            view
                >> BeautifulExample.view
                    { title = "Game of Life"
                    , details =
                        Just
                            """
                      Check the code and tests out in the Github repository.
                      Open a issue to help me doing improvements and documentation.
                    """
                    , color = Just Color.blue
                    , maxWidth = 478
                    , githubUrl = Just "https://github.com/gabrielperales/game-of-life"
                    , documentationUrl = Nothing
                    }
        , init = init
        , update = update
        , subscriptions = subscriptions
        }



-- TYPES DEFINITION


type alias BoardShape =
    ( Int, Int )


type alias CellCoords =
    ( Int, Int )


type alias Board =
    Array Cell


type alias Cell =
    Bool



-- MODEL


type alias Model =
    { shape : BoardShape
    , cells : Board
    , paused : Bool
    }



-- HELPER FUNCTIONS


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
        , paused = True
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
        ( y, x )


coordsToIndex : BoardShape -> CellCoords -> Int
coordsToIndex shape coords =
    let
        width =
            shape |> Tuple.first

        height =
            shape |> Tuple.second

        y =
            coords
                |> Tuple.first
                |> \n ->
                    if height > 0 then
                        n % height
                    else
                        n

        x =
            coords
                |> Tuple.second
                |> \n ->
                    if width > 0 then
                        n % width
                    else
                        n
    in
        (y * width + x)


neighbors : CellCoords -> List CellCoords
neighbors cellCoords =
    [ ( -1, -1 )
    , ( -1, 0 )
    , ( -1, 1 )
    , ( 0, -1 )
    , ( 0, 1 )
    , ( 1, -1 )
    , ( 1, 0 )
    , ( 1, 1 )
    ]
        |> List.map
            (\t ->
                ( (Tuple.first t) + (Tuple.first cellCoords)
                , (Tuple.second t) + (Tuple.second cellCoords)
                )
            )


neighborsIndexes : BoardShape -> List CellCoords -> List Int
neighborsIndexes shape coords =
    List.map (coordsToIndex shape) coords


neighborsAlive : Board -> List Int -> Int
neighborsAlive board indexes =
    indexes
        |> List.filterMap (\i -> Array.get i board)
        |> List.filter ((==) True)
        |> List.length


isAlive : Bool -> Int -> Bool
isAlive alive neighbors =
    alive && neighbors == 2 || neighbors == 3



-- UPDATE


type Msg
    = Update
    | Init Board
    | SwitchCell Int
    | SwitchPlay


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Update ->
            if (not <| .paused <| model) then
                ( lifeCycle model, Cmd.none )
            else
                ( model, Cmd.none )

        Init cells ->
            ( { model | cells = cells }, Cmd.none )

        SwitchCell cellIndex ->
            let
                cells =
                    model
                        |> .cells
                        |> Array.indexedMap
                            (\i cell ->
                                if (i /= cellIndex) then
                                    cell
                                else
                                    not cell
                            )
            in
                ( { model | cells = cells }, Cmd.none )

        SwitchPlay ->
            ( { model | paused = not model.paused }, Cmd.none )


lifeCycle : Model -> Model
lifeCycle board =
    let
        boardShape =
            .shape board

        boardCells =
            .cells board

        nextCells =
            boardCells
                |> Array.indexedMap
                    (\index cell ->
                        index
                            |> indexToCoords boardShape
                            |> neighbors
                            |> neighborsIndexes boardShape
                            |> neighborsAlive boardCells
                            |> isAlive cell
                    )
    in
        { board | cells = nextCells }



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
        div []
            [ cells
                |> Array.toList
                |> List.indexedMap (\i c -> [ input [ id ("cell-" ++ toString i), type_ "checkbox", checked c, onClick (SwitchCell i) ] [] ])
                |> List.map (td [])
                |> split width
                |> List.map (tr [])
                |> table []
            , button [ onClick SwitchPlay ]
                [ text
                    (if model.paused then
                        "Play"
                     else
                        "Pause"
                    )
                ]
            ]



-- SUBCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second (\_ -> Update)


init : ( Model, Cmd Msg )
init =
    ( empty ( 20, 20 ), Random.generate Init (Random.map (Array.fromList) (Random.list 400 Random.bool)) )
