data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
  where
    halfAlphabet = alphabetSize `div` 2
    offset = fromEnum c + halfAlphabet
    rotation = offset `mod` alphabetSize

largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)

message :: [FourLetterAlphabet]
message = [L1, L3, L4, L1, L1, L2]

fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder xs = map rot4l xs
  where
    rot4l = rotN alphabetSize
    alphabetSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)

threeLetterAlphabetEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterAlphabetEncoder xs = map rot3l xs
  where
    rot3l = rotN alphabetSize
    alphabetSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)

data ThreeLetterAlphabet = Alpha | Beta | Kappa deriving (Show, Enum, Bounded)

m1 :: [ThreeLetterAlphabet] = [Alpha, Alpha, Beta, Alpha, Kappa]

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder alphabetSize c = toEnum rotation
  where
    halfAlphabet = alphabetSize `div` 2
    offset = if even alphabetSize then fromEnum c + halfAlphabet else 1 + fromEnum c + halfAlphabet
    rotation = offset `mod` alphabetSize

threeLetterAlphabetDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterAlphabetDecoder xs = map rot3ldecoder xs
  where
    rot3ldecoder = rotNdecoder alphabetSize
    alphabetSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)

-- Final encoder and decoder

rotEncoder :: String -> String
rotEncoder text = map rotChar text
  where
    rotChar = rotN alphabetSize
    alphabetSize = 1 + fromEnum (maxBound :: Char)

rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text
  where
    rotCharDecoder = rotNdecoder alphabetSize
    alphabetSize = 1 + fromEnum (maxBound :: Char)

secretMessage = "Jean-Paul likes Simone"
encodedMessage = rotEncoder secretMessage
decodedMessage = rotDecoder encodedMessage
