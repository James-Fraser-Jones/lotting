import Browser exposing (Document, document)
import Browser.Dom as Dom exposing (Error, focus, setViewport)
import File exposing (File)
import File.Select as Select exposing (file)
import File.Download as Download exposing (string)
import Html exposing (Html, Attribute, button, text, div, table, thead, tbody, tr, th, td, textarea, input, section)
import Html.Events exposing (onClick, onInput, on, keyCode, custom)
import Html.Attributes exposing (disabled, value, class, placeholder, autofocus, id, style)
import Html.Lazy as Lazy exposing (lazy)
import Task exposing (perform, attempt)
import Csv as C exposing (Csv, parse)
import Maybe exposing (withDefault)
import Json.Decode as Decode exposing (map)

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
  { csv : C.Csv
  , selected : Point
  , fileName : String
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
  = Done          --Generic
  | Error String

  | CsvRequested  --Csv
  | CsvSelected File
  | CsvLoaded String String
  | CsvRemoved
  | CsvExported

  | CellSelected Int Int  --Cell
  | CellEdited String

  | FilenameEdited String --Filename

  | KeyPressed Key

type Key
  = Up
  | Down
  | Left
  | Right
  | Enter
  | Tab
  | Other

-- INIT

init : () -> (Model, Cmd Msg)
init _ =
  (Model Nothing, Cmd.none)

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    Done ->
      ( model
      , Cmd.none
      )

    Error message ->
      ( model
      , Cmd.none
      )

    CsvRequested ->
      ( model
      , Select.file [ csv_mime ] CsvSelected
      )

    CsvSelected file ->
      ( model
      , Task.perform (CsvLoaded <| File.name file) (File.toString file)
      )

    CsvLoaded fileName fileContent ->
      ( { model | data = Maybe.map (\x -> LoadedCsv x (Point 0 0) fileName) (C.parse fileContent |> silence) }
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

    KeyPressed key ->
      let newModel = { model | data = Maybe.map (updateKey key) model.data }
      in ( newModel
         , Maybe.withDefault Cmd.none <| Maybe.map findRownum newModel.data
         )

findRownum : LoadedCsv -> Cmd Msg
findRownum loadedCsv =
  focusCursor loadedCsv.selected.row

focusCursor : Int -> Cmd Msg
focusCursor rowNum =
  Cmd.batch [ Dom.focus cursor_id |> Task.attempt handleError
            , if rowNum == 0 then resetViewport else Cmd.none
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
  in  Download.string (if loadedCsv.fileName == "" then "export.csv" else loadedCsv.fileName) csv_mime file

resetViewport : Cmd Msg
resetViewport = Task.perform (\_ -> Done) (Dom.setViewport 0 0)

handleError : Result Error () -> Msg
handleError result =
  case result of
    Err (Dom.NotFound message) -> Error message
    Ok _ -> Done

keyMapper : Int -> Key
keyMapper n =
  case n of
    9 -> Tab
    13 -> Enter
    37 -> Left
    38 -> Up
    39 -> Right
    40 -> Down
    _ -> Other

updateKey : Key -> LoadedCsv -> LoadedCsv
updateKey key loadedCsv =
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
    Other -> loadedCsv

-- VIEW

docView : Model -> Document Msg
docView = Lazy.lazy view >> List.singleton >> Document "Smart Lotter"

view : Model -> Html Msg
view model =
  div [ id "all" ]
    [ section [ class "section" ]
        [ div [ class "container" ]
            [ div [ class "columns" ]
                [ div [ class "column is-one-quarter" ]
                    [ div [ class "buttons has-addons" ]
                        [ button [ onClick CsvRequested, class "button is-primary" ] [ text "Load CSV" ]
                        , button [ onClick CsvRemoved, disabled (model.data == Nothing), class "button is-danger" ] [ text "Remove" ]
                        , button [ onClick CsvExported, disabled (model.data == Nothing), class "button is-info" ] [ text "Export" ]
                        ]
                    ]
                , div [ class "column is-one-fifth" ]
                    [ input [ value <| Maybe.withDefault "" <| Maybe.map (.fileName) model.data,
                              disabled (model.data == Nothing),
                              onInput FilenameEdited,
                              class "input",
                              placeholder "File Name" ] []
                    ]
                ]
            ]
        ]
    , section [ class "section" ]
        [ div [ class "container" ]
            [ case model.data of
                Nothing -> div [] []
                Just loadedCsv -> createTable loadedCsv
            ]
        ]
    ]

createTable : LoadedCsv -> Html Msg
createTable loadedCsv =
  table [ class "table is-bordered is-striped is-hoverable is-fullwidth table-fixed"
        , on "keydown" (Decode.map (keyMapper >> KeyPressed) keyCode)
        ] [createHead loadedCsv.csv.headers, createBody loadedCsv.selected loadedCsv.csv.records]

createHead : List String -> Html Msg
createHead = List.map (text >> List.singleton >> th []) >> tr [] >> List.singleton >> thead []

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
subscriptions _ =
  Sub.none

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
