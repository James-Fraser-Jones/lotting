import Browser exposing (Document, document)
import Browser.Dom exposing (Error(..), focus, setViewport, setViewportOf, getViewportOf)
import File exposing (File)
import File.Select exposing (file)
import File.Download exposing (string)
import Html exposing (Html, Attribute, button, text, div, table, thead, tbody, tr, th, td, textarea, input, section, h2)
import Html.Events exposing (onClick, onInput, on, keyCode, custom, onMouseDown, onMouseUp)
import Html.Attributes exposing (disabled, value, class, placeholder, autofocus, id, style)
import Html.Lazy exposing (lazy)
import Task exposing (perform, attempt)
import Csv exposing (Csv, parse)
import Maybe exposing (withDefault)
import Json.Decode exposing (map)
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key exposing (Key(..))

-- MAIN

main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = docView
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { data : Maybe LoadedCsv
  }

type alias LoadedCsv =
  { csv : Csv
  , selected : Point
  , fileName : String
  , colWidths : List Int
  , resizing : Maybe Int
  }

type alias Point =
  { row : Int
  , col : Int
  }

-- type alias Csv =
--     { headers : List String
--     , records : List (List String)
--     }

type Msg
--Generic
  = NoOp
  | HandleErrorEvent String
  | HandleKeyboardEvent String KeyboardEvent
--Csv
  | CsvRequested
  | CsvSelected File
  | CsvLoaded String String
  | CsvRemoved
  | CsvExported
--Cell
  | CellSelected Int Int
  | CellEdited String
--Filename
  | FilenameEdited String
--Column resizing
  | StartColumnResize Int
  | ColumnResize
  | EndColumnResize

-- INIT

init : () -> (Model, Cmd Msg)
init _ = (Model Nothing, Cmd.none)

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    NoOp ->
      ( model
      , Cmd.none
      )

    HandleErrorEvent message ->
      ( print message model
      , Cmd.none
      )

    HandleKeyboardEvent message keyboardEvent ->
      case message of
        "movingCursor" -> let newModel = { model | data = Maybe.map (moveCursor keyboardEvent.keyCode) model.data }
                          in ( newModel
                             , Maybe.withDefault Cmd.none <| Maybe.map findRownum newModel.data
                             )
        _ -> ( model
             , Cmd.none
             )

    CsvRequested ->
      ( model
      , file [ csv_mime ] CsvSelected
      )

    CsvSelected file ->
      ( model
      , Task.perform (CsvLoaded <| File.name file) (File.toString file)
      )

    CsvLoaded fileName fileContent ->
      ( { model | data = Maybe.map (\x -> LoadedCsv x (Point 0 0) fileName (List.map (always 300) x.headers) Nothing) (parse fileContent |> silence) }
      , focusCursor 0
      )

    CsvRemoved ->
      ( { model | data = Nothing }
      , Cmd.none
      )

    CsvExported ->
      ( model
      , Maybe.withDefault Cmd.none <| Maybe.map exportCsv model.data
      )

    CellSelected rowNum colNum ->
      ( { model | data = Maybe.map (newSelected (Point rowNum colNum)) model.data }
      , focusCursor rowNum
      )

    CellEdited newText ->
      ( { model | data = Maybe.map (newEdit newText) model.data }
      , Cmd.none
      )

    FilenameEdited newText ->
      ( { model | data = Maybe.map (newFilename newText) model.data }
      , Cmd.none
      )

    StartColumnResize colNum ->
      ( { model | data = Maybe.map (updateResizing <| Just colNum) model.data }
      , Cmd.none
      )

    ColumnResize ->
      ( model
      , Cmd.none
      )

    EndColumnResize ->
      ( { model | data = Maybe.map (updateResizing Nothing) model.data }
      , Cmd.none
      )

findRownum : LoadedCsv -> Cmd Msg
findRownum loadedCsv =
  focusCursor loadedCsv.selected.row

focusCursor : Int -> Cmd Msg
focusCursor rowNum =
  Cmd.batch [ focus cursor_id |> Task.attempt handleError
            , if rowNum == 0 then jumpToTop "tableViewport" else Cmd.none
            ]

newSelected : Point -> LoadedCsv -> LoadedCsv
newSelected point loadedCsv = {loadedCsv | selected = point}

newEdit : String -> LoadedCsv -> LoadedCsv
newEdit str loadedCsv =
  let updateRecord = updateAt loadedCsv.selected.col (always str)
      newRecords = updateAt loadedCsv.selected.row updateRecord loadedCsv.csv.records
      oldCsv = loadedCsv.csv
      newCsv = {oldCsv | records = newRecords}
   in {loadedCsv | csv = newCsv}

newFilename : String -> LoadedCsv -> LoadedCsv
newFilename str loadedCsv = { loadedCsv | fileName = str }

exportCsv : LoadedCsv -> Cmd Msg
exportCsv loadedCsv =
  let unwrappedCsv = loadedCsv.csv.headers :: loadedCsv.csv.records
      file = List.map (String.join ",") unwrappedCsv |> String.join windows_newline
  in  string (if loadedCsv.fileName == "" then "export.csv" else loadedCsv.fileName) csv_mime file

jumpToTop : String -> Cmd Msg
jumpToTop viewPortId =
  getViewportOf viewPortId
    |> Task.andThen (\info -> setViewportOf viewPortId info.viewport.x 0)
    |> Task.attempt handleError

handleError : Result Error () -> Msg
handleError result =
  case result of
    Err (NotFound message) -> HandleErrorEvent message
    Ok _ -> NoOp

moveCursor : Key -> LoadedCsv -> LoadedCsv
moveCursor key loadedCsv =
  let old = loadedCsv.selected
      lastX = List.length loadedCsv.csv.headers - 1
      lastY = List.length loadedCsv.csv.records - 1
  in case key of
    Tab -> { loadedCsv | selected = Point old.row (min (old.col + 1) lastX) }
    Enter -> { loadedCsv | selected = Point (min (old.row + 1) lastY) old.col }
    Left -> { loadedCsv | selected = Point old.row (max (old.col - 1) 0) }
    Up -> { loadedCsv | selected = Point (max (old.row - 1) 0) old.col }
    Right -> { loadedCsv | selected = Point old.row (min (old.col + 1) lastX) }
    Down -> { loadedCsv | selected = Point (min (old.row + 1) lastY) old.col }
    _ -> loadedCsv

updateResizing : Maybe Int -> LoadedCsv -> LoadedCsv
updateResizing m loadedCsv =
  { loadedCsv | resizing = printt m }

-- VIEW

docView : Model -> Document Msg
docView = lazy view >> List.singleton >> Document "Smart Lotter"

view : Model -> Html Msg
view model =
  div [ id "all" ]
    [ div [ class "columns" ]
        [ div [ class "column is-one-quarter section" ]
            [ h2 [ class "title is-2" ] [ text "Smart Lotter" ]
            , div [ class "buttons has-addons" ]
                [ button [ onClick CsvRequested, class "button is-primary" ] [ text "Open" ]
                , button [ onClick CsvRemoved, disabled (model.data == Nothing), class "button is-danger" ] [ text "Clear" ]
                , button [ onClick CsvExported, disabled (model.data == Nothing), class "button is-info" ] [ text "Save" ]
                ]
            , input
                [ value <| Maybe.withDefault "" <| Maybe.map (.fileName) model.data
                ,  disabled (model.data == Nothing)
                ,  onInput FilenameEdited
                ,  class "input"
                ,  placeholder "File Name"
                ] []
            ]
        , createTable model.data
        ]
    ]

createTable : Maybe LoadedCsv -> Html Msg
createTable data =
  case data of
    Nothing ->
      div [ class "table-empty column is-three-quarters" ] []
    Just loadedCsv ->
      div [ class "table-fixed column is-three-quarters", id "tableViewport" ]
        [ table
            [ class "table is-bordered is-striped is-hoverable is-fullwidth"
            , onKeyboardEvent "keydown" "movingCursor"
            ]
            [ createHead loadedCsv.colWidths loadedCsv.csv.headers
            , createBody loadedCsv.selected loadedCsv.csv.records
            ]
        ]

onKeyboardEvent : String -> String -> Attribute Msg
onKeyboardEvent eventName message =
  on eventName <| map (HandleKeyboardEvent message) decodeKeyboardEvent

createHead : List Int -> List String -> Html Msg
createHead widths elems = zipWith (|>) widths (List.indexedMap createHeadCell elems) |> tr [] |> List.singleton |> thead []

createHeadCell : Int -> String -> Int -> Html Msg
createHeadCell colNum elem weight =
  [ text elem
  , div
      [ class "slider"
      , onMouseDown <| StartColumnResize colNum
      , onMouseUp EndColumnResize
      ] []
  ] |> th [style "width" (String.fromInt weight ++ "px")]

createBody : Point -> List (List String) -> Html Msg
createBody point rows =
  List.indexedMap (createRow point) rows |> tbody []

createRow : Point -> Int -> (List String) -> Html Msg
createRow point rowNum elems =
  List.indexedMap (createCell point rowNum) elems |> tr []

createCell : Point -> Int -> Int -> String -> Html Msg
createCell point rowNum colNum elem =
  if Point rowNum colNum == point
  then td [ class "cursorCell" ] [ input [ value elem, onInput CellEdited, class "input", id cursor_id ] [] ]
  else td [ onClick (CellSelected rowNum colNum) ] [ text elem ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- PRELUDE

silence : Result e a -> Maybe a
silence result =
  case result of
    Ok value -> Just value
    Err _ -> Nothing

print : a -> b -> b
print a b = always b <| Debug.log "" (Debug.toString a)

printt : a -> a
printt a = print a a

flip : (a -> b -> c) -> b -> a -> c
flip f a b = f b a

curry : (a -> b -> c) -> (a, b) -> c
curry f (a, b) = f a b

uncurry : ((a, b) -> c) -> a -> b -> c
uncurry f a b = f (a, b)

zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith f a b =
  case (a, b) of
    ([], _) -> []
    (_, []) -> []
    (x :: xs, y :: ys) -> f x y :: zipWith f xs ys

updateAt : Int -> (a -> a) -> List a -> List a
updateAt n f lst =
  case (n, lst) of
    (_, []) -> []
    (0, (x :: xs)) -> f x :: xs
    (nn, (x :: xs)) -> x :: updateAt (nn - 1) f xs

-- CONSTS

windows_newline : String
windows_newline = "\r\n"

csv_mime : String
csv_mime = "text/csv"

cursor_id : String
cursor_id = "cursor"
