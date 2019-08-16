module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Html exposing (Html, br, button, div, text)
import Json.Decode as Decode
import Svg exposing (circle, path, svg)
import Svg.Attributes exposing (cx, cy, d, fill, height, r, stroke, strokeWidth, transform, width)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- CONSTANCES


rotationSpeed : Int
rotationSpeed =
    3


maxSpeed : Float
maxSpeed =
    20


backgroundColor =
    "#ffffff"



-- TYPES


type Msg
    = Rotate Direction
    | Shield Bool
    | Shoot Bool
    | Forwards
    | None


type Direction
    = Left
    | Right


type alias Vec2 =
    ( Float, Float )


type alias Model =
    { rotation : Int
    , speed : Vec2
    , position : Vec2
    , shield : Bool
    , shooting : Bool
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { rotation = 0
      , speed = ( 0, 0 )
      , position = ( 0, 0 )
      , shield = False
      , shooting = False
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Rotate dir ->
            case dir of
                Left ->
                    ( { model | rotation = modBy 360 (model.rotation - rotationSpeed) }, Cmd.none )

                Right ->
                    ( { model | rotation = modBy 360 (model.rotation + rotationSpeed) }, Cmd.none )

        Forwards ->
            ( { model | speed = calcSpeed model.speed model.rotation }, Cmd.none )

        Shield state ->
            ( { model | shield = state }, Cmd.none )

        Shoot state ->
            ( { model | shooting = state }, Cmd.none )

        None ->
            ( model, Cmd.none )


calcSpeed : Vec2 -> Int -> Vec2
calcSpeed ( speedX, speedY ) rotation =
    ( inAbsoluteRange maxSpeed (speedX + sin (degrees (toFloat rotation)))
    , inAbsoluteRange maxSpeed (speedY + cos (degrees (toFloat rotation)))
    )


inAbsoluteRange : Float -> Float -> Float
inAbsoluteRange absoluteValue value =
    max -absoluteValue (min value absoluteValue)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ spaceShip model
        , debugModel model
        ]


spaceShip : Model -> Html Msg
spaceShip model =
    svg
        [ width "35"
        , height "35"
        ]
        [ spaceShipShield model.shield
        , circle [ cx "17.5", cy "17.5", r "17.25", fill backgroundColor ] []
        , path
            [ d "M30 28.75L17.5 23.76L5 28.75L17.5 0.75L30 28.75Z"
            , fill "#000000"
            , strokeWidth "0"
            , transform (String.concat [ "rotate(", String.fromInt model.rotation, ", 17.5, 17.5)" ])
            ]
            []
        ]


debugModel : Model -> Html Msg
debugModel model =
    div []
        [ text (String.concat [ "Rotation: ", String.fromInt model.rotation ])
        , br [] []
        , text (String.concat [ "Cos rotation: ", String.fromFloat (cos (degrees (toFloat model.rotation))) ])
        , br [] []
        , text (String.concat [ "Sin rotation: ", String.fromFloat (sin (degrees (toFloat model.rotation))) ])
        , br [] []
        , text (String.concat [ "Speed: x = ", String.fromFloat (Tuple.first model.speed), ", y = ", String.fromFloat (Tuple.second model.speed) ])
        , br [] []
        , text (String.concat [ "Shield: ", boolToString model.shield ])
        , br [] []
        , text (String.concat [ "Shoot: ", boolToString model.shooting ])
        ]


boolToString : Bool -> String
boolToString bool =
    case bool of
        True ->
            "True"

        False ->
            "False"


spaceShipShield : Bool -> Html Msg
spaceShipShield shield =
    case shield of
        True ->
            circle [ cx "17.5", cy "17.5", r "17.5" ] []

        False ->
            circle [ cx "17.5", cy "17.5", r "17.5", fill backgroundColor ] []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (keyDecoder keyDownToMsg)
        , onKeyUp (keyDecoder keyUpToMsg)
        ]


keyDecoder : (String -> Msg) -> Decode.Decoder Msg
keyDecoder callback =
    Decode.map callback (Decode.field "code" Decode.string)


keyDownToMsg : String -> Msg
keyDownToMsg string =
    case string of
        "ArrowLeft" ->
            Rotate Left

        "ArrowRight" ->
            Rotate Right

        "ArrowUp" ->
            Forwards

        "Space" ->
            Shoot True

        "ShiftLeft" ->
            Shield True

        _ ->
            None


keyUpToMsg : String -> Msg
keyUpToMsg string =
    case string of
        "ShiftLeft" ->
            Shield False

        "Space" ->
            Shoot False

        _ ->
            None
