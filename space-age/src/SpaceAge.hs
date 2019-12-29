module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

yearInSecondsInPlanet :: Planet -> Float
yearInSecondsInPlanet = secondsToYear . getFactor

secondsToYear :: Float -> Float
secondsToYear = (* 31557600)

getFactor :: Planet -> Float
getFactor Mercury = 0.2408467
getFactor Venus = 0.61519726
getFactor Earth = 1 
getFactor Mars = 1.8808158 
getFactor Jupiter = 11.862615
getFactor Saturn = 29.447498
getFactor Uranus = 84.016846 
getFactor Neptune = 164.79132

ageOn :: Planet -> Float -> Float
ageOn p = (/ getFactor p) . (* 31557600)