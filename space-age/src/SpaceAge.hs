module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
  | Venus
  | Earth
  | Mars
  | Jupiter
  | Saturn
  | Uranus
  | Neptune

orbitalPeriodInEarthYears :: Planet -> Float
orbitalPeriodInEarthYears Mercury = 0.2408467
orbitalPeriodInEarthYears Venus = 0.61519726
orbitalPeriodInEarthYears Earth = 1.0
orbitalPeriodInEarthYears Mars = 1.8808158
orbitalPeriodInEarthYears Jupiter = 11.862615
orbitalPeriodInEarthYears Saturn = 29.447498
orbitalPeriodInEarthYears Uranus = 84.016846
orbitalPeriodInEarthYears Neptune = 164.79132

earthOrbitalPeriodInSeconds = 31557600

orbitalPeriodInSeconds :: Planet -> Float
orbitalPeriodInSeconds planet = (orbitalPeriodInEarthYears planet) * earthOrbitalPeriodInSeconds

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / (orbitalPeriodInSeconds planet)
