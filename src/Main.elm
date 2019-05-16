module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Collage exposing (..)
import Collage.Events exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (..)
import Color
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import List exposing (..)
import Math.Vector2 exposing (..)
import Particle as P
import Platform exposing (..)
import Random
import Task
import Time



{- GRAVITY -}


g : Vec2
g =
    vec2 0 15



{- PARTICLE MASS -}


mass : Float
mass =
    999



{- DRAG COEFF TOTAL -}
{- Drag = Cd * .5 * r * V^2 * A -}
{- Cd: drag coeff, r: air density -}
{- Cd * .5 * r -}


d : Float
d =
    0.001



{- BASE PARTICLES -}


cantParticles : Float
cantParticles =
    1


type alias ViewportInfo =
    { width : Float
    , height : Float
    }


toViewPortInfo : Viewport -> ViewportInfo
toViewPortInfo v =
    { width = v.viewport.width, height = v.viewport.height }


type GameState
    = Win
    | Lose
    | Playing
    | Loading


type alias GameInfo =
    { gameState : GameState }


type alias MetaInfo =
    { frameCount : Float
    , frameCountAcc : Float
    , fps : Float
    , startTime : Int
    , currentTime : Int
    , viewport : ViewportInfo
    }


type alias Model =
    { metaInfo : MetaInfo
    , particles : List P.Particle
    , gameInfo : GameInfo
    }


type Msg
    = Frame Float
    | Tick1s Time.Posix
    | Tick20ms Time.Posix
    | ViewportInfoUpdate ViewportInfo
    | InitParticlePositions ViewportInfo
    | InitStartTime Time.Posix
    | ParticleClicked Int


setGameInfo : GameInfo -> Model -> Model
setGameInfo gi m =
    { m | gameInfo = gi }


setGameState : GameState -> GameInfo -> GameInfo
setGameState gs gi =
    { gi | gameState = gs }


setParticles : List P.Particle -> Model -> Model
setParticles ps m =
    { m | particles = ps }


setMetaInfo : MetaInfo -> Model -> Model
setMetaInfo f m =
    { m | metaInfo = f }


setFrameCount : Float -> MetaInfo -> MetaInfo
setFrameCount c f =
    { f | frameCount = c }


setStartTime : Int -> MetaInfo -> MetaInfo
setStartTime st f =
    { f | startTime = st }


setCurrentTime : Int -> MetaInfo -> MetaInfo
setCurrentTime t f =
    { f | currentTime = t }


increaseFrameCount : MetaInfo -> MetaInfo
increaseFrameCount f =
    { f | frameCount = f.frameCount + 1, frameCountAcc = f.frameCountAcc + 1 }


setViewportInfo : ViewportInfo -> MetaInfo -> MetaInfo
setViewportInfo vInfo m =
    { m | viewport = vInfo }


calcFps : MetaInfo -> MetaInfo
calcFps f =
    { f | frameCount = 0, fps = f.frameCount }


didPlayerLose : Model -> Bool
didPlayerLose { metaInfo, particles } =
    any
        (\p ->
            getY p.p
                + p.radius
                > metaInfo.viewport.height
                || getY p.p
                - p.radius
                < 0
                || getX p.p
                + p.radius
                > metaInfo.viewport.width
                || getX p.p
                - p.radius
                < 0
        )
        particles



{- ------------------------------------------------------------------------------------ -}
{- MAIN -}


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



{- ------------------------------------------------------------------------------------ -}
{- INIT -}


createInitParticles : Float -> MetaInfo -> List P.Particle
createInitParticles cant { viewport } =
    let
        pBase =
            map toFloat (range 0 (round (cant - 1)))
    in
    map
        (\x ->
            P.createParticle
                (vec2 x 0)
                (vec2 0 1)
                mass
                (round x)
        )
        pBase


init : () -> ( Model, Cmd Msg )
init _ =
    ( { metaInfo =
            { frameCount = 0
            , frameCountAcc = 0
            , fps = 60
            , startTime = 0
            , currentTime = 0
            , viewport =
                { width = 0
                , height = 0
                }
            }
      , particles = []
      , gameInfo = { gameState = Loading }
      }
    , Cmd.batch
        [ getViewport |> Task.perform (\v -> ViewportInfoUpdate (toViewPortInfo v))
        , getViewport |> Task.perform (\v -> InitParticlePositions (toViewPortInfo v))
        , Time.now |> Task.perform (\t -> InitStartTime t)
        ]
    )



{- ------------------------------------------------------------------------------------ -}
{- UPDATE -}


update msg model =
    case msg of
        ViewportInfoUpdate viewport ->
            let
                newModel =
                    model
                        |> setMetaInfo (setViewportInfo viewport model.metaInfo)
            in
            ( newModel, Cmd.none )

        InitParticlePositions viewport ->
            let
                marginX =
                    viewport.width * 0.1

                widthP =
                    (viewport.width - (2 * marginX)) / cantParticles

                getNewX x =
                    marginX + ((x + 0.5) * widthP)

                heightP =
                    viewport.height / 10

                newModel =
                    model
                        |> setParticles
                            (map
                                (\p -> P.setPos (vec2 (getNewX <| getX p.p) heightP) p)
                                model.particles
                            )
                        |> setGameInfo (setGameState Playing model.gameInfo)
            in
            ( newModel, Cmd.none )

        Frame _ ->
            let
                newCmd =
                    if didPlayerLose model then
                        getViewport |> Task.perform (\v -> InitParticlePositions (toViewPortInfo v))

                    else
                        Cmd.none

                newParticles =
                    if didPlayerLose model then
                        createInitParticles cantParticles model.metaInfo

                    else
                        model.particles

                newGameState =
                    if didPlayerLose model then
                        Loading

                    else
                        model.gameInfo.gameState

                newModel =
                    model
                        |> setMetaInfo (increaseFrameCount model.metaInfo)
                        |> setParticles newParticles
                        |> setGameInfo (setGameState newGameState model.gameInfo)
            in
            ( newModel, newCmd )

        InitStartTime pTime ->
            let
                newModel =
                    model
                        |> setMetaInfo (setStartTime (Time.posixToMillis pTime) model.metaInfo)
                        |> setMetaInfo (setCurrentTime (Time.posixToMillis pTime) model.metaInfo)
            in
            ( newModel, Cmd.none )

        Tick1s _ ->
            let
                newModel =
                    model
                        |> setMetaInfo (calcFps model.metaInfo)
            in
            ( newModel, Cmd.none )

        Tick20ms pTime ->
            let
                newModel =
                    if model.gameInfo.gameState == Loading then
                        model

                    else
                        model
                            |> setParticles
                                (P.updateWorld
                                    g
                                    d
                                    (Time.posixToMillis pTime - model.metaInfo.currentTime)
                                    model.particles
                                )
                            |> setMetaInfo (setCurrentTime (Time.posixToMillis pTime) model.metaInfo)
            in
            ( newModel, Cmd.none )

        ParticleClicked id ->
            let
                newModel =
                    model
                        |> setParticles
                            (P.invertVelById g id model.particles)
            in
            ( newModel, Cmd.none )



{- ------------------------------------------------------------------------------------ -}
{- SUBSCRIPTIONS -}


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta Frame
        , onResize (\w h -> ViewportInfoUpdate { width = toFloat w, height = toFloat h })
        , Time.every 1000 Tick1s
        , Time.every 20 Tick20ms
        ]



{- ------------------------------------------------------------------------------------ -}
{- VIEW -}


view : Model -> Html Msg
view model =
    div
        [ style "width" "100%", style "height" "100%" ]
        [ viewSvg model
        , viewFPS model.metaInfo.fps
        ]


viewFPS fps =
    div
        [ style "position" "fixed"
        , style "top" "1rem"
        , style "right" "1rem"
        , style "z-index" "10"
        ]
        [ text <| String.fromFloat fps ]


viewSvg : Model -> Html Msg
viewSvg model =
    impose
        (renderParticles model.metaInfo.frameCountAcc model.particles)
        (renderBackground model.metaInfo |> align topLeft)
        |> svgExplicit [ style "width" "100%", style "height" "100%" ]


renderBackground : MetaInfo -> Collage msg
renderBackground m =
    let
        hue =
            toFloat (m.frameCountAcc / 8 |> floor |> modBy 100) / 100
    in
    rectangle m.viewport.width m.viewport.height
        |> filled (uniform <| Color.hsl hue 0.1 0.9)


drawParticle : Float -> P.Particle -> Collage Msg
drawParticle count p =
    let
        hue =
            toFloat (count / 4 |> floor |> modBy 100) / 100
    in
    circle p.radius
        |> filled (uniform <| Color.hsl hue 0.8 0.7)
        |> shift ( getX p.p, -1 * getY p.p )
        |> onClick (ParticleClicked p.id)


renderParticles : Float -> List P.Particle -> Collage Msg
renderParticles frameCount ps =
    group <| map (drawParticle frameCount) ps
