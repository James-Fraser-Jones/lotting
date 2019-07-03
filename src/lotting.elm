import Browser exposing (Document, document)
import File exposing (File)
import File.Select as Select exposing (file)
import File.Download as Download exposing (string)
import Html exposing (Html, button, text, div, table, tr, th, td, textarea, input, section)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (disabled, value, class, placeholder)
import Task exposing (perform)
import Csv as C exposing (Csv, parse)
import Maybe exposing (withDefault)

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
  = CsvRequested
  | CsvSelected File
  | CsvLoaded String String
  | CsvRemoved
  | CsvExported
  | CellSelected Int Int
  | CellEdited String
  | FilenameEdited String

-- INIT

init : () -> (Model, Cmd Msg)
init _ =
  (Model Nothing, Cmd.none)

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CsvRequested ->
      ( model
      , Select.file ["text/csv"] CsvSelected
      )

    CsvSelected file ->
      ( model
      , Task.perform (CsvLoaded <| File.name file) (File.toString file)
      )

    CsvLoaded fileName fileContent ->
      ( { model | data = Maybe.map (\x -> LoadedCsv x (Point 0 0) fileName) (C.parse fileContent |> silence) }
      , Cmd.none
      )

    CsvRemoved ->
      ( { model | data = Nothing }
      , Cmd.none
      )

    CellSelected rowNum colNum ->
      ( { model | data = Maybe.map (newSelected (Point rowNum colNum)) model.data }
      , Cmd.none
      )

    CellEdited newText ->
      ( { model | data = Maybe.map (newEdit newText) model.data }
      , Cmd.none
      )

    CsvExported ->
      ( model
      , Maybe.withDefault Cmd.none <| Maybe.map exportCsv model.data
      )

    FilenameEdited newText ->
      ( { model | data = Maybe.map (newFilename newText) model.data }
      , Cmd.none
      )

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
  in  Download.string loadedCsv.fileName "text/csv" file

-- VIEW

docView : Model -> Document Msg
docView = Document "Smart Lotter" << List.singleton << view

view : Model -> Html Msg
view model =
  div []
    [ section [ class "section" ]
        [ div [ class "container" ]
            [ div [ class "columns" ]
                [ div [ class "column is-one-quarter" ]
                    [ div [ class "buttons has-addons" ]
                        [ button [ onClick CsvRequested, class "button is-primary" ] [ text "Load CSV" ]
                        , button [ onClick CsvRemoved, disabled (model.data == Nothing), class "button is-danger" ] [ text "Remove CSV" ]
                        , button [ onClick CsvExported, disabled (model.data == Nothing), class "button is-info" ] [ text "Export CSV" ]
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
  table [ class "table" ] (createHeaderRow loadedCsv.csv.headers :: createRows loadedCsv.selected loadedCsv.csv.records)

createHeaderRow : List String -> Html Msg
createHeaderRow =
  List.map (text >> List.singleton >> th []) >> tr []

createRows : Point -> List (List String) -> List (Html Msg)
createRows point rows =
  List.indexedMap (createRow point) rows

createRow : Point -> Int -> (List String) -> Html Msg
createRow point rowNum elems =
  List.indexedMap (createCell point rowNum) elems |> tr []

createCell : Point -> Int -> Int -> String -> Html Msg
createCell point rowNum colNum elem =
  if Point rowNum colNum == point
  then input [value elem, onInput CellEdited, class "input" ] []
  else td [onClick (CellSelected rowNum colNum)] [text elem]

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

windows_newline : String
windows_newline = "\r\n"
