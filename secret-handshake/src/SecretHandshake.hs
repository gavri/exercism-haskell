module SecretHandshake (handshake) where

import Data.Bits

commandFromBitPosition :: Int -> [String] -> [String]
commandFromBitPosition 0 = (++ ["wink"])
commandFromBitPosition 1 = (++ ["double blink"])
commandFromBitPosition 2 = (++ ["close your eyes"])
commandFromBitPosition 3 = (++ ["jump"])
commandFromBitPosition 4 = reverse

handshake :: Int -> [String]
handshake n = foldr ($) [] commands
  where commands = map commandFromBitPosition bitPositionsThatAreSet
        bitPositionsThatAreSet = filter isBitSet bitPositions
        bitPositions = [4, 3, 2, 1, 0]
        isBitSet m = masked m /= 0
        masked m = n .&. bitMask m
        bitMask = shift 1
