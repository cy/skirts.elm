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
  }


model : Model
model =
  Model 27.5 34.5 0



-- UPDATE
safeToFloat = String.toFloat >> Result.toMaybe >> Maybe.withDefault 0

type Msg
    = Waist String
    | Hip String
    | Length String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Waist waist ->
      { model | waist = safeToFloat waist }

    Hip hip ->
      { model | hip = safeToFloat hip }

    Length length ->
      { model | length = safeToFloat length }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ type_ "number", step "0.25", placeholder "Waist", defaultValue (toString model.waist), onInput Waist ] []
    , input [ type_ "number", step "0.25", placeholder "Hip", defaultValue (toString model.hip), onInput Hip ] []
    , input [ type_ "number", step "0.25", placeholder "Length", defaultValue (toString model.length), onInput Length ] []
    , calculateSkirt model
    ]


seamAllowance = 5 / 8
hemWidth = 1 / 2
ease = 5 / 8
bandWidth = 2 + (1 / 4)
calculateSkirt: Model -> Html measurements
calculateSkirt model =
  let
    frontWidth = (model.hip / 2) + seamAllowance
    frontLength = model.length + seamAllowance + (2 * hemWidth)
    backWidth = (model.hip / 2) + 2 * seamAllowance
    backLength = frontLength
    waistBandWidth = (bandWidth) + 2 * seamAllowance
    waistBandLength = (model.waist / 2) + seamAllowance + ease
  in
    ul [] [
      li [] [ text (toString frontWidth) ]
    , li [] [ text (toString frontLength) ]
    , li [] [ text (toString backWidth) ]
    , li [] [ text (toString backLength) ]
    , li [] [ text (toString waistBandWidth) ]
    , li [] [ text (toString waistBandLength) ]
  ]
