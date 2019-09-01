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


fireRateAfterShoot : Float
fireRateAfterShoot =
    200


fireLifeTime : Float
fireLifeTime =
    1200



-- TYPES


type Msg
    = Rotate Direction
    | Shield Bool
    | Shoot Bool
    | Accelerate Bool
    | UpdateFrame Float
    | None


type Direction
    = Left
    | Right


type LifeTime
    = Finite Float
    | Infinite


type alias ShipModel =
    { rotation : Float
    , move : Movable
    , shield : Bool
    , engine : Bool
    , fireRate : Float
    }


type alias Movable =
    { vel : Vec2
    , pos : Vec2
    , kind : MovableKind
    , timeLeft : LifeTime
    }


type alias Vec2 =
    { x : Float, y : Float }


type MovableKind
    = Ship
    | ShipFire
    | Asteroid
    | Enemy
    | EnemyFire


type alias Model =
    { ship : ShipModel
    , shipFire : List Movable
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { ship =
            { rotation = 0
            , move =
                { vel = { x = 0, y = 0 }
                , pos = { x = toFloat mapDimensions.x / 2, y = toFloat mapDimensions.y / 2 }
                , kind = Ship
                , timeLeft = Infinite
                }
            , shield = False
            , engine = False
            , fireRate = 0
            }
      , shipFire = []
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    let
        modelShip =
            model.ship

        canShoot =
            model.ship.fireRate == 0
    in
    case msg of
        Rotate dir ->
            ( { model | ship = updateShipRotation modelShip dir }, Cmd.none )

        Accelerate engine ->
            ( { model | ship = updateShipSpeed modelShip engine }, Cmd.none )

        UpdateFrame deltaTime ->
            ( updateFrame model deltaTime, Cmd.none )

        Shield state ->
            ( { model | ship = updateShipShield modelShip state }, Cmd.none )

        Shoot fireButtonPressed ->
            case fireButtonPressed && canShoot of
                True ->
                    ( { model
                        | shipFire = updateShipFire model
                        , ship = { modelShip | fireRate = fireRateAfterShoot }
                      }
                    , Cmd.none
                    )

                False ->
                    ( model, Cmd.none )

        None ->
            ( model, Cmd.none )


updateShipRotation : ShipModel -> Direction -> ShipModel
updateShipRotation shipModel dir =
    case dir of
        Left ->
            { shipModel | rotation = modByFloat 360 (shipModel.rotation - rotationSpeed) }

        Right ->
            { shipModel | rotation = modByFloat 360 (shipModel.rotation + rotationSpeed) }


updateShipSpeed : ShipModel -> Bool -> ShipModel
updateShipSpeed shipModel engine =
    let
        move =
            shipModel.move

        newVel =
            { x = move.vel.x + acceleration * sin (degrees shipModel.rotation)
            , y = move.vel.y - acceleration * cos (degrees shipModel.rotation)
            }

        newMove =
            { move | vel = newVel }
    in
    { shipModel | move = newMove, engine = engine }


updateFrame : Model -> Float -> Model
updateFrame model deltaTime =
    { model
        | ship = updateShip model.ship deltaTime
        , shipFire =
            model.shipFire
                |> List.map (\x -> updateLifeTime x deltaTime)
                |> List.filter isAlive
                |> List.map (\x -> updatePosition x deltaTime)
    }


updateShip : ShipModel -> Float -> ShipModel
updateShip shipModel deltaTime =
    { shipModel
        | move = updatePosition shipModel.move deltaTime
        , fireRate = max 0 (shipModel.fireRate - deltaTime)
    }


updatePosition : Movable -> Float -> Movable
updatePosition move deltaTime =
    let
        newPos =
            { x = inNonNegativeRange mapDimensions.x (move.pos.x + move.vel.x * deltaTime)
            , y = inNonNegativeRange mapDimensions.y (move.pos.y + move.vel.y * deltaTime)
            }
    in
    { move | pos = newPos }


isAlive : Movable -> Bool
isAlive move =
    case move.timeLeft of
        Infinite ->
            True

        Finite left ->
            left > 0


updateLifeTime : Movable -> Float -> Movable
updateLifeTime move deltaTime =
    { move
        | timeLeft =
            case move.timeLeft of
                Infinite ->
                    Infinite

                Finite left ->
                    Finite (left - deltaTime)
    }


updateShipShield : ShipModel -> Bool -> ShipModel
updateShipShield shipModel state =
    { shipModel | shield = state }


updateShipFire : Model -> List Movable
updateShipFire { ship, shipFire } =
    let
        { move, rotation } =
            ship

        radians =
            degrees rotation

        vel =
            { x = move.vel.x + 0.3 * sin radians
            , y = move.vel.y - 0.3 * cos radians
            }
    in
    shipFire ++ [ { kind = ShipFire, pos = move.pos, vel = vel, timeLeft = Finite fireLifeTime } ]


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
        [ spaceShip model.ship
        , renderMovableObjects model.shipFire
        , debugShipModel model.ship
        ]


spaceShip : ShipModel -> Html Msg
spaceShip shipModel =
    let
        radiusString =
            String.fromFloat spaceShipRadius

        xTransform =
            formatFloat (shipModel.move.pos.x - spaceShipRadius)

        yTransform =
            formatFloat (shipModel.move.pos.y - spaceShipRadius)

        rotation =
            formatFloat shipModel.rotation

        diameterString =
            String.fromFloat (spaceShipRadius * 2)

        shieldOpacity =
            getOpacity shipModel.shield

        engineGlowOpacity =
            getOpacity shipModel.engine
    in
    div [ style (String.concat [ "position: absolute; width: ", diameterString, "px; height: ", diameterString, "px; transform: translate(", xTransform, "px, ", yTransform, "px);" ]) ]
        [ svg
            [ width diameterString
            , height diameterString
            ]
            [ circle [ cx "17.5", cy "17.5", r "17.5", fill "#000000", opacity shieldOpacity ] []
            , circle [ cx radiusString, cy radiusString, r "17", fill "#ffffff" ] []
            , g [ transform (String.concat [ "rotate(", rotation, ", ", radiusString, ", ", radiusString, ")" ]) ]
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


renderMovableObjects : List Movable -> Html Msg
renderMovableObjects list =
    div [] (List.map renderMovableObject list)


renderMovableObject : Movable -> Html Msg
renderMovableObject object =
    case object.kind of
        ShipFire ->
            div [ style <| String.concat [ "position: absolute; width: 5px; height: 5px; background-color: #000; border-radius: 50%; transform: translate(", String.fromFloat (object.pos.x - 2.5), "px, ", String.fromFloat (object.pos.y - 2.5), "px);" ] ] []

        _ ->
            div [] []


getOpacity : Bool -> String
getOpacity condition =
    case condition of
        True ->
            "1"

        False ->
            "0"


debugShipModel : ShipModel -> Html Msg
debugShipModel shipModel =
    div [ style "position: fixed; bottom: 0px; left: 0px;" ]
        [ text (String.concat [ "Rotation: ", String.fromFloat shipModel.rotation ])
        , br [] []
        , text (String.concat [ "Cos rotation: ", String.fromFloat -(cos (degrees shipModel.rotation)) ])
        , br [] []
        , text (String.concat [ "Sin rotation: ", String.fromFloat (sin (degrees shipModel.rotation)) ])
        , br [] []
        , text (String.concat [ "Position: x = ", String.fromFloat shipModel.move.pos.x, ", y = ", String.fromFloat shipModel.move.pos.y ])
        , br [] []
        , text (String.concat [ "Speed: x = ", String.fromFloat shipModel.move.vel.x, ", y = ", String.fromFloat shipModel.move.vel.y ])
        , br [] []
        , text (String.concat [ "Shield: ", boolToString shipModel.shield ])
        , br [] []
        , text (String.concat [ "Shoot: ", String.fromFloat shipModel.fireRate ])
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
    UpdateFrame deltaTime


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
    base + rest


formatFloat : Float -> String
formatFloat value =
    value
        |> toFixed 4
        |> String.fromFloat


toFixed : Int -> Float -> Float
toFixed fixedPos =
    roundNum fixedPos
