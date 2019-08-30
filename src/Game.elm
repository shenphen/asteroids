module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Html exposing (Html, br, button, div, text)
import Json.Decode as Decode
import Round exposing (roundNum)
import Svg exposing (circle, g, path, svg)
import Svg.Attributes exposing (cx, cy, d, fill, height, opacity, r, stroke, strokeWidth, style, transform, width)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- CONSTANCES


mapDimensions : { x : Int, y : Int }
mapDimensions =
    { x = 800, y = 600 }


rotationSpeed : Float
rotationSpeed =
    7.4335


acceleration : Float
acceleration =
    0.0211


maxSpeed : Float
maxSpeed =
    0.7


spaceShipRadius : Float
spaceShipRadius =
    17.5



-- TYPES


type Msg
    = Rotate Direction
    | Shield Bool
    | Shoot Bool
    | Accelerate Bool
    | UpdatePosition Float
    | None


type Direction
    = Left
    | Right


type alias Vec2 =
    { x : Float, y : Float }


type alias Model =
    { rotation : Float
    , speed : Vec2
    , position : Vec2
    , shield : Bool
    , shooting : Bool
    , engine : Bool
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { rotation = 0
      , speed = { x = 0, y = 0 }
      , position = { x = toFloat mapDimensions.x / 2, y = toFloat mapDimensions.y / 2 }
      , shield = False
      , shooting = False
      , engine = False
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
                    ( { model | rotation = modByFloat 360 (model.rotation - rotationSpeed) }, Cmd.none )

                Right ->
                    ( { model | rotation = modByFloat 360 (model.rotation + rotationSpeed) }, Cmd.none )

        Accelerate engine ->
            ( { model | speed = updateSpeed model.speed model.rotation, engine = engine }, Cmd.none )

        UpdatePosition deltaTime ->
            ( { model | position = updatePosition model deltaTime }, Cmd.none )

        Shield state ->
            ( { model | shield = state }, Cmd.none )

        Shoot state ->
            ( { model | shooting = state }, Cmd.none )

        None ->
            ( model, Cmd.none )


updateSpeed : Vec2 -> Float -> Vec2
updateSpeed speed rotation =
    { x = (speed.x + acceleration * sin (degrees rotation)) |> toFixed 4
    , y = (speed.y - acceleration * cos (degrees rotation)) |> toFixed 4
    }


inAbsoluteRange : Float -> Float -> Float
inAbsoluteRange absoluteValue value =
    max -absoluteValue (min value absoluteValue)


updatePosition : Model -> Float -> Vec2
updatePosition model deltaTime =
    let
        updatedX =
            model.position.x + model.speed.x * deltaTime

        updatedY =
            model.position.y + model.speed.y * deltaTime
    in
    { x = updatedX |> inNonNegativeRange mapDimensions.x
    , y = updatedY |> inNonNegativeRange mapDimensions.y
    }


inNonNegativeRange : Int -> Float -> Float
inNonNegativeRange maxValue value =
    modByFloat maxValue value



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style
            (String.concat
                [ "position: relative; overflow: hidden; border: solid 1px #000; width: "
                , String.fromInt mapDimensions.x
                , "px; height: "
                , String.fromInt mapDimensions.y
                , "px;"
                ]
            )
        ]
        [ spaceShip model
        , debugModel model
        ]


spaceShip : Model -> Html Msg
spaceShip model =
    let
        radiusString =
            String.fromFloat spaceShipRadius

        xTransform =
            String.fromFloat (model.position.x - spaceShipRadius)

        yTransform =
            String.fromFloat (model.position.y - spaceShipRadius)

        diameterString =
            String.fromFloat (spaceShipRadius * 2)

        shieldOpacity =
            getOpacity model.shield

        engineGlowOpacity =
            getOpacity model.engine
    in
    div [ style (String.concat [ "transform: translate(", xTransform, "px, ", yTransform, "px);" ]) ]
        [ svg
            [ width diameterString
            , height diameterString
            ]
            [ circle [ cx "17.5", cy "17.5", r "17.5", fill "#000000", opacity shieldOpacity ] []
            , circle [ cx radiusString, cy radiusString, r "17", fill "#ffffff" ] []
            , g [ transform (String.concat [ "rotate(", String.fromFloat model.rotation, ", ", radiusString, ", ", radiusString, ")" ]) ]
                [ path
                    [ d "M15.05 26.58L17.09 30.65L19.14 26.58L21.19 32.33L21.19 20.83L13 20.83L13 32.33L15.05 26.58Z"
                    , fill "#2d9cda"
                    , stroke "#000000"
                    , strokeWidth "0.5"
                    , opacity engineGlowOpacity
                    ]
                    []
                , path
                    [ d "M30 28.75L17.5 23.76L5 28.75L17.5 0.75L30 28.75Z"
                    , fill "#000000"
                    , strokeWidth "0"
                    ]
                    []
                ]
            ]
        ]


getOpacity : Bool -> String
getOpacity condition =
    case condition of
        True ->
            "1"

        False ->
            "0"


debugModel : Model -> Html Msg
debugModel model =
    div [ style "position: fixed; bottom: 0px; left: 0px;" ]
        [ text (String.concat [ "Rotation: ", String.fromFloat model.rotation ])
        , br [] []
        , text (String.concat [ "Cos rotation: ", String.fromFloat -(cos (degrees model.rotation)) ])
        , br [] []
        , text (String.concat [ "Sin rotation: ", String.fromFloat (sin (degrees model.rotation)) ])
        , br [] []
        , text (String.concat [ "Position: x = ", String.fromFloat model.position.x, ", y = ", String.fromFloat model.position.y ])
        , br [] []
        , text (String.concat [ "Speed: x = ", String.fromFloat model.speed.x, ", y = ", String.fromFloat model.speed.y ])
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (keyDecoder keyDownToMsg)
        , onKeyUp (keyDecoder keyUpToMsg)
        , onAnimationFrameDelta onAnimationUpdate
        ]


onAnimationUpdate : Float -> Msg
onAnimationUpdate deltaTime =
    UpdatePosition deltaTime


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
            Accelerate True

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

        "ArrowUp" ->
            Accelerate False

        "Space" ->
            Shoot False

        _ ->
            None



-- HELPERS


modByFloat : Int -> Float -> Float
modByFloat modulo value =
    let
        intValue =
            floor value

        base =
            modBy modulo intValue
                |> toFloat

        rest =
            abs (value - toFloat intValue)
    in
    base + toFixed 4 rest


toFixed : Int -> Float -> Float
toFixed fixedPos =
    roundNum fixedPos
