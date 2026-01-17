-------------------------------------------------------------------------------------
--------------------------------------- ROT13 ---------------------------------------
-------------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------------
---------------------------------------- XOR ----------------------------------------
-------------------------------------------------------------------------------------

xorBool :: Bool -> Bool -> Bool
xorBool x y = (x || y) && not (x && y)

xorPair :: (Bool, Bool) -> Bool
xorPair (x, y) = xorBool x y

xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair (zip list1 list2)

-- Bits
type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = intToBits' (n `div` 2) ++ [odd n]

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = missingBits ++ originalBitsList
  where
    missingBits = take leadingFalsesNumber (repeat False)
    leadingFalsesNumber = maxBits - length originalBitsList
    originalBitsList = intToBits' n

charToBits :: Char -> Bits
charToBits c = intToBits (fromEnum c)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2 ^ snd x) trueLocations)
  where
    indices = [size - 1, size - 2 .. 0]
    size = length bits
    trueLocations = filter (\x -> fst x == True) (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

-- One-time pad
myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plainText = map (\pair -> xor (fst pair) (snd pair)) bitPairs
  where
    bitPairs = zip padBits plainTextBits
    padBits = map charToBits pad
    plainTextBits = map charToBits plainText

applyOTP :: String -> String -> String
applyOTP pad plainText = map bitsToChar (applyOTP' pad plainText)

-- Encoder/decoder
encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

ex1 = encoderDecoder "book"

-------------------------------------------------------------------------------------
----------------------------------- CIPHER CLASS ------------------------------------
-------------------------------------------------------------------------------------

class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
  encode Rot text = rotEncoder text
  decode Rot text = rotDecoder text

exRotEncoded = encode Rot "Haskell"

exRotDecoded = decode Rot exRotEncoded

data OneTimePad = OTP String

instance Cipher OneTimePad where
  encode (OTP pad) text = applyOTP pad text
  decode (OTP pad) text = applyOTP pad text

-- Creating a limitless pad
myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

exOtpEncoded = encode myOTP "Learn Haskell"

exOtpDecoded = decode myOTP exOtpEncoded

exTwo = encode myOTP "this is a longer sentence, I hope it encodes"

exTwoDecoded = decode myOTP exTwo

-- Pseudo-random number generator

-- A linear conguential PRNG
prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a * seed + b) `mod` maxNumber

examplePRNgLessThan100 = prng 1337 7 100

firstR = examplePRNgLessThan100 12345

-- Homework: StreamCipher

printableChar :: Int -> Char
printableChar n = toEnum (32 + n `mod` 95)

generatePseudoRandomChar :: Int -> Int -> Int -> Int -> Char
generatePseudoRandomChar a b maxNumber seed = printableChar (prng a b maxNumber seed)

generatePseudoRandomString :: Int -> Int -> Int -> Int -> Int -> String
generatePseudoRandomString 0 a b maxNumber seed = ""
generatePseudoRandomString stringLength a b maxNumber seed =
  generatedChar : generatePseudoRandomString (stringLength - 1) a b maxNumber nextSeed
  where
    generatedChar = printableChar nextSeed
    nextSeed = prng a b maxNumber seed

data PRNG = PRNG Int Int Int Int

data StreamCipher = SC PRNG

instance Cipher StreamCipher where
  encode (SC (PRNG a b maxNumber seed)) text = applyOTP pad text
    where
      textLength = length text
      pad = generatePseudoRandomString textLength a b maxNumber seed

  decode (SC (PRNG a b maxNumber seed)) text = applyOTP pad text
    where
      textLength = length text
      pad = generatePseudoRandomString textLength a b maxNumber seed

rndString = generatePseudoRandomString 50 1337 7 100 12345

mySc = SC (PRNG 1337 7 maxBound 12345)

exEncodedWithStreamCipher = encode mySc "Let's get programming with Haskell"

exDecodedWithStreamCipher = decode mySc exEncodedWithStreamCipher