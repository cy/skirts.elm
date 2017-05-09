import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Regex

main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model =
  { waist : Float
  , hip: Float
  , length: Float
  , bandWidth: Float
  }


model : Model
model =
  Model 27.5 34.5 19 (2 + (1 / 4))



-- UPDATE
safeToFloat = String.toFloat >> Result.toMaybe >> Maybe.withDefault 0
safeToInt = String.toInt >> Result.toMaybe >> Maybe.withDefault 0

maybeToInt : Maybe Int -> Int
maybeToInt m =
  case m of
    Just i -> i
    Nothing -> 0

type Msg
    = Waist String
    | Hip String
    | Length String
    | BandWidth String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Waist waist ->
      { model | waist = safeToFloat waist }

    Hip hip ->
      { model | hip = safeToFloat hip }

    Length length ->
      { model | length = safeToFloat length }

    BandWidth bandWidth ->
      { model | bandWidth = safeToFloat bandWidth }


-- VIEW

withStyle html =
  div []
  [ node "style" [type_ "text/css"] [text "@import url(https://cdnjs.cloudflare.com/ajax/libs/normalize/5.0.0/normalize.min.css)"]
  --, node "style" [type_ "text/css"] [text "@import url(http://localhost:8000/styles.css)"]
  --, node "style" [] [text "@import url(https://fonts.googleapis.com/css?family=Indie+Flower|Courgette)"]
  , html
  ]

inputStyle = [ ("width", "60px")
  , ("margin", "0 5px 0 5px")
  , ("border-radius", "5px 5px 5px 5px")
  , ("border", "1px solid #ccc")
  , ("font-family", "Courgette")
  , ("padding-right", "2px")
  ]
view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "skirts.🌳"]
    , h2 [] [ text "✨ Clemence Gathered Skirt"]
    , label [] [ text "Waist" ]
    , input [ type_ "number", step "0.25", placeholder "Waist", size 4, style inputStyle, defaultValue (toString model.waist), onInput Waist ] []
    , br [] []
    , label [] [ text "Hip" ]
    , input [ type_ "number", step "0.25", placeholder "Hip", size 4, style inputStyle, defaultValue (toString model.hip), onInput Hip ] []
    , br [] []
    , label [] [ text "Waist to Hem Length"]
    , input [ type_ "number", step "0.25", placeholder "Length", size 4, style inputStyle, defaultValue (toString model.length), onInput Length ] []
    , br [] []
    , label [] [ text "Waistband Width"]
    , input [ type_ "number", step "0.25", placeholder "Length", size 4, style inputStyle, defaultValue (toString model.bandWidth), onInput BandWidth ] []
    , calculateSkirt model
    ]
  |> withStyle

toNearestEighth: Float -> String
toNearestEighth input =
  let
    inputString = toString input
    dotIndices = String.indices "." inputString
    firstIndex = List.head dotIndices
    decimalString = String.slice (maybeToInt firstIndex) (String.length inputString) inputString
    decimal = safeToFloat decimalString
    wholeNumberString = String.slice 0 (maybeToInt firstIndex) inputString
    nearestEigth = ceiling(decimal * 8)
  in
    wholeNumberString ++ " " ++ toString nearestEigth ++ "/8"

seamAllowance = 5 / 8
hemWidth = 1 / 2
ease = 5 / 8
calculateSkirt: Model -> Html measurements
calculateSkirt model =
  let
    frontWidth = (model.hip / 2) + seamAllowance
    frontLength = model.length + seamAllowance + (2 * hemWidth)
    backWidth = (model.hip / 2) + 2 * seamAllowance
    backLength = frontLength
    waistBandWidth = (model.bandWidth) + 2 * seamAllowance
    waistBandLength = (model.waist / 2) + seamAllowance + ease
    frontYardage = frontWidth * frontLength * 2
    backYardage = backWidth * backLength * 2
  in
    ul [] [
      li [] [ text ("Front Width " ++ (toString frontWidth) ++ (" (" ++ toNearestEighth frontWidth ++ ")")) ]
    , li [] [ text ("Front Length " ++ (toString frontLength) ++ (" (" ++ toNearestEighth frontLength ++ ")")) ]
    , li [] [ text ("Back Width " ++ (toString backWidth) ++ (" (" ++ toNearestEighth backWidth ++ ")")) ]
    , li [] [ text ("Back Length " ++ (toString backLength) ++ (" (" ++ toNearestEighth backLength ++ ")")) ]
    , li [] [ text ("Waistband Width " ++ (toString waistBandWidth) ++ (" (" ++ toNearestEighth waistBandWidth ++ ")")) ]
    , li [] [ text ("Waistband Length " ++ (toString waistBandLength) ++ (" (" ++ toNearestEighth waistBandLength ++ ")")) ]
  ]
