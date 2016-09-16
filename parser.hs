import Text.Parsec
import qualified Text.ParserCombinators.Parsec.Token as PT


numberExamples = [("1", 1),
                  ("23", 23)]

number :: Parser Integer

number = do
  n <- (many1 digit)
  return (read n)

parseWith :: Parsec String [u] a -> String -> Either ParseError a
parseWith p = runParser p [] "untyped lambda-calculus"

main = do
  line <- getLine
  putStrLn $ show $ parseWith number line

  
