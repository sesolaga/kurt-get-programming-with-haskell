data AuthorName = AuthorName
  { firstName :: String,
    lastName :: String
  }

type FirstName = String

type MiddleName = String

type LastName = String

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitialsWithLast Char Char LastName
  | FirstNameWithTwoInitials FirstName Char Char
  deriving (Show)

data Creator = AuthorCreator Author | ArtistCreator Artist deriving (Show)

data Author = Author Name deriving (Show)

data Artist = Person Name | Band String deriving (Show)

hpLoveCraft :: Creator
hpLoveCraft =
  AuthorCreator
    ( Author
        (TwoInitialsWithLast 'H' 'P' "Lovecraft")
    )

data Book = Book
  { author :: Creator,
    isbn :: String,
    bookTitle :: String,
    bookYear :: Int,
    bookPrice :: Double
  }

data VinylRecord = VinylRecord
  { artist :: Creator,
    recordTitle :: String,
    recordYear :: Int,
    recordPrice :: Double
  }

data CollectibleToy = CollectibleToy
  { name :: String,
    description :: String,
    toyPrice :: Double
  }

data Pamthlet = Pamthlet
  { title :: String,
    pamthletDescription :: String,
    contact :: String
  }

data StoreItem = BookItem Book | RecordItem VinylRecord | ToyItem CollectibleToy | PamthletItem Pamthlet

price :: StoreItem -> Double
price (BookItem x) = bookPrice x
price (RecordItem x) = recordPrice x
price (ToyItem x) = toyPrice x
price (PamthletItem _) = 0

madeBy :: StoreItem -> String
madeBy (BookItem x) = show (author x)
madeBy (RecordItem x) = show (artist x)
madeBy (PamthletItem x) = contact x
madeBy _ = "unknown"

b1 :: StoreItem
b1 =
  BookItem
    Book
      { author = hpLoveCraft,
        isbn = "ISBN-TEST-123",
        bookTitle = "Cthulhu",
        bookYear = 1914,
        bookPrice = 23
      }

b1MadeBy = madeBy b1

-- Homework
data Shape = CircleShape Circle | SquareShape Square | RectangleShape Rectangle

data Circle = Circle
  { radius :: Double
  }

data Square = Square
  { side :: Double
  }

data Rectangle = Rectangle
  { sideA :: Double,
    sideB :: Double
  }

perimeter :: Shape -> Double
perimeter (CircleShape x) = 2 * pi * radius x
perimeter (SquareShape x) = 4 * side x
perimeter (RectangleShape x) = 2 * (sideA x + sideB x)

area :: Shape -> Double
area (CircleShape x) = pi * (radius x) ^ 2
area (SquareShape x) = (side x) ^ 2
area (RectangleShape x) = sideA x * sideB x

c = CircleShape Circle {radius = 5}

pc = perimeter c

ac = area c

r =
  RectangleShape
    Rectangle
      { sideA = 4,
        sideB = 3
      }

pr = perimeter r

ar = area r