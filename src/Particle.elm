module Particle exposing (Particle, createParticle, invertVelById, moveEm, setPos, updateWorld)

import List exposing (..)
import Math.Vector2 exposing (..)
import Tuple exposing (..)


type alias Tick =
    Int


type alias Particle =
    { p : Vec2
    , v : Vec2
    , radius : Float
    , mass : Float
    , restitution : Float
    , id : Int
    }


areVecsEqual : Vec2 -> Vec2 -> Bool
areVecsEqual a b =
    (getX a == getX b) && (getY a == getY b)


areVecsSameDirection : Vec2 -> Vec2 -> Bool
areVecsSameDirection a b =
    (sign (getX a) == sign (getX b)) && (sign (getY a) == sign (getY b))


sign : Float -> Float
sign a =
    if a < 0 then
        -1

    else
        1


setPos : Vec2 -> Particle -> Particle
setPos p e =
    { e | p = p }


setVel : Vec2 -> Particle -> Particle
setVel v e =
    { e | v = v }


createParticle : Vec2 -> Vec2 -> Float -> Int -> Particle
createParticle pos vel mass id =
    { p = pos
    , v = vel
    , radius = 15
    , mass = mass
    , restitution = 5
    , id = id
    }


invertVelById : Vec2 -> Int -> List Particle -> List Particle
invertVelById a id =
    map
        (\p ->
            if p.id == id then
                { p | v = scale 2 (Math.Vector2.negate a) }

            else
                p
        )


updateWorld : Vec2 -> Float -> Tick -> List Particle -> List Particle
updateWorld g d t ps =
    map (moveEm g d t) ps
        |> collideEm


moveEm : Vec2 -> Float -> Tick -> Particle -> Particle
moveEm g dCoeff t e =
    let
        surface =
            e.radius * e.radius * 2 * 3.14

        drag =
            scale (dCoeff * surface) (scale (dot e.v e.v) (normalize e.v))

        weight =
            scale e.mass g

        diff =
            sub weight drag

        wdX =
            if getX diff < 0 then
                0

            else
                getX diff

        wdY =
            if getY diff < 0 then
                0

            else
                getY diff

        wd =
            vec2 wdX wdY

        aFinal =
            scale (1 / e.mass) wd

        {- _ = Debug.log "Vel" e.v
           _ = Debug.log "drag" drag
           _ = Debug.log "diff" diff
           _ = Debug.log "wd" wd
           _ = Debug.log "aFinal" aFinal
        -}
    in
    { e
        | p = nextPosition aFinal t e
        , v = nextVelocity aFinal t e
    }



{-
   moveEm : Vec2 -> Float -> Tick -> Particle -> Particle
   moveEm a dCoeff t e =
       { e
           | p = nextPosition a t e
           , v = nextVelocity a t e
       }
-}


nextPosition : Vec2 -> Tick -> Particle -> Vec2
nextPosition a t e =
    scale (((toFloat t / 1000) ^ 2) / 2) a
        |> add (scale (toFloat t / 1000) e.v)
        |> add e.p


nextVelocity : Vec2 -> Tick -> Particle -> Vec2
nextVelocity a t e =
    scale (toFloat t / 1000) a
        |> add e.v


collideEm : List Particle -> List Particle
collideEm ps =
    let
        detected =
            map (\p -> ( p, detectCollisions ps p )) ps
    in
    {- resolveCollisions detected -}
    ps


detectCollisions : List Particle -> Particle -> List Particle
detectCollisions xs y =
    foldl
        (\x acc ->
            if collided x y == True then
                x :: acc

            else
                acc
        )
        []
        xs


collided : Particle -> Particle -> Bool
collided a b =
    let
        r =
            a.radius + b.radius

        d =
            distance a.p b.p
    in
    r > d


resolveCollisions : List ( Particle, List Particle ) -> List Particle
resolveCollisions ps =
    map
        (\( x, xs ) -> resolveCollision x xs)
        ps


resolveCollision : Particle -> List Particle -> Particle
resolveCollision a bs =
    let
        b =
            { mass = foldl (\c acc -> c.mass + acc) 0 bs
            , v = foldl (\c acc -> add c.v acc) (vec2 0 0) bs
            }

        rv =
            sub b.v a.v

        normal =
            normalize rv

        velAlongNormal =
            dot rv normal

        e =
            a.restitution

        j =
            (-(1 + e) * velAlongNormal) / (a.mass + 1 / b.mass)

        impulse =
            scale j normal
    in
    if velAlongNormal > 0 then
        a

    else
        setVel (sub a.v (scale (1 / a.mass) impulse)) a
