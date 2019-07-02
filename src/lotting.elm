import Browser exposing (element)
import File exposing (File)
import File.Select as Select exposing (file)
import File.Download as Download exposing (string)
import Html exposing (Html, button, text, div, table, tr, th, td, textarea)
import Html.Events exposing (onClick, onInput)
import Task exposing (perform)
import Csv as C exposing (Csv, parse)
import Maybe exposing (withDefault)

-- MAIN

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
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
  | CsvLoaded String
  | CsvRemoved
  | CsvExported
  | CellSelected Int Int
  | CellEdited String

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
      , Task.perform CsvLoaded (File.toString file)
      )

    CsvLoaded content ->
      ( { model | data = Maybe.map (flip LoadedCsv (Point 0 0)) (C.parse content |> silence) }
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

newSelected : Point -> LoadedCsv -> LoadedCsv
newSelected point loadedCsv = { loadedCsv | selected = point }

newEdit : String -> LoadedCsv -> LoadedCsv
newEdit str loadedCsv =
  let updateRecord = updateAt loadedCsv.selected.col (always str)
      newRecords = updateAt loadedCsv.selected.row updateRecord loadedCsv.csv.records
      oldCsv = loadedCsv.csv
      newCsv = { oldCsv | records = newRecords }
   in { loadedCsv | csv = newCsv }

exportCsv : LoadedCsv -> Cmd Msg
exportCsv loadedCsv =
  let unwrappedCsv = loadedCsv.csv.headers :: loadedCsv.csv.records
      file = List.map (String.join ",") unwrappedCsv |> String.join "\r\n"
  in  Download.string "export.csv" "text/csv" file

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick CsvRequested ] [ text "Load CSV" ]
    , button [ onClick CsvRemoved ] [ text "Remove CSV" ]
    , button [ onClick CsvExported ] [ text "Export CSV" ]
    , case model.data of
        Nothing -> div [] []
        Just loadedCsv -> createTable loadedCsv
    ]

createTable : LoadedCsv -> Html Msg
createTable loadedCsv =
  table [] (createHeaderRow loadedCsv.csv.headers :: createRows loadedCsv.selected loadedCsv.csv.records)

createHeaderRow : List String -> Html Msg
createHeaderRow =
  List.map (text >> List.singleton >> th []) >> tr []

createRows : Point -> List (List String) -> List (Html Msg)
createRows point rows =
  zipWith (createRow point) rows (List.range 0 (List.length rows - 1))

createRow : Point -> (List String) -> Int -> Html Msg
createRow point elems rowNum =
  tr [] (zipWith (createCell point rowNum) (List.range 0 (List.length elems - 1)) elems)

createCell : Point -> Int -> Int -> String -> Html Msg
createCell point rowNum colNum elem =
  if Point rowNum colNum == point
  then textarea [onInput CellEdited] [text elem]
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
