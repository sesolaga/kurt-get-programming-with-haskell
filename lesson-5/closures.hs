ifEven f x =
  if even x
    then f x
    else x

genIfEven f = (\x -> ifEven f x)

genIfXEven x = (\f -> ifEven f x)

getRequestUrl host apiKey resource id =
  host
    ++ "/"
    ++ resource
    ++ "/"
    ++ id
    ++ "?token="
    ++ apiKey

genHostRequestBuilder host = (\apiKey resource id -> getRequestUrl host apiKey resource id)

genApiRequestBuilder hostBuilder apiKey = (\resource id -> hostBuilder apiKey resource id)

genResourceRequestBuilder apiBuilder resource = (\id -> apiBuilder resource id)

-- Partial application
exampleUrlBuilder = getRequestUrl "http://example.com"

apiUrlBuilder = exampleUrlBuilder "secret"

booksUrlBuilder = apiUrlBuilder "books"

anotherBooksUrlBuilder = getRequestUrl "http://example.com" "secret" "books"

flipBinaryArgs binaryFunction = (\x y -> binaryFunction y x)

subtract2 = flipBinaryArgs (-) 2

-- Q 5.1
ifEvenInc = ifEven (\x -> x + 1)
ifEvenDouble = ifEven (\x -> x * 2)
ifEvenSquare = ifEven (\x -> x ^ 2)

-- Q 5.2
binaryPartialApplication binaryFunction x = (\y -> binaryFunction x y)