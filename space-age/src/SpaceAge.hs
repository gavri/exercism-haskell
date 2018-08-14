module SpaceAge (Planet(..), ageOn) where

import Data.Maybe

data Planet = Mercury
  | Venus
  | Earth
  | Mars
  | Jupiter
  | Saturn
  | Uranus
  | Neptune deriving Eq

orbitalPeriodInEarthYears :: Planet -> Float
orbitalPeriodInEarthYears planet = fromMaybe 1.0 $ lookup planet orbitalPeriodInEarthYearsMap
  where
    orbitalPeriodInEarthYearsMap = [
      (Mercury, 0.2408467),
      (Venus, 0.61519726),
      (Mars, 1.8808158),
      (Jupiter, 11.862615),
      (Saturn, 29.447498),
      (Uranus, 84.016846),
      (Neptune, 164.79132)]

earthOrbitalPeriodInSeconds = 31557600

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / orbitalPeriodInSeconds
  where
    orbitalPeriodInSeconds = orbitalPeriodInEarthYears planet * earthOrbitalPeriodInSeconds
