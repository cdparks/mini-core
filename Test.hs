module TestFormat where
import Parse

-- Test formatting with a few different constructions
main = do
    source <- readFile "./Test.core"
    putStrLn $ showParse source

