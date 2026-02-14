import qualified Data.Map as Map

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

main :: IO ()
main = do
  putStrLn "Hello! What's your name?"
  name <- getLine
  let statement = helloPerson name
  putStrLn statement

-- Homework

userInput :: Map.Map Int String
userInput = Map.fromList [(1, "Sergei")]

maybeHello :: Maybe String
maybeHello = do
  name <- Map.lookup 1 userInput
  let statement = helloPerson name
  return statement
