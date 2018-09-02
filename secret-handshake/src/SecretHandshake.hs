module SecretHandshake (handshake) where

import Data.Bits
import Data.Function

commandFromBitPosition :: Int -> [String] -> [String]
commandFromBitPosition 0 = (++ ["wink"])
commandFromBitPosition 1 = (++ ["double blink"])
commandFromBitPosition 2 = (++ ["close your eyes"])
commandFromBitPosition 3 = (++ ["jump"])
commandFromBitPosition 4 = reverse

handshake :: Int -> [String]
handshake n = foldl (&) [] commands
  where commands = map commandFromBitPosition setBits
        setBits = filter isBitSet bitPositions
        isBitSet m = n .&. (shift 1 m) /= 0
        bitPositions = [0, 1, 2, 3, 4]
