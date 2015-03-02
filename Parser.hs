module Parser (main) where
import Lexer



main :: IO ()
main = do
	putStr $ show $ tokenize "test" "(Hello {-Hello-} 5) -- More comment"