module Parse where
import Data.Char
import Data.List
import Format

type Token = (Int, String)

-- Scan string with look-ahead 1
scan :: Int -> String -> [Token]
scan lineno [] = [(lineno, "")]
scan lineno s@(c:cs)
    | c == '\n' = scan (lineno + 1) cs
    | isSpace c = scan lineno cs
    | isDigit c = let (digits, rest) = span isDigit s
                  in (lineno, digits):scan lineno rest
    | isAlpha c = let (ending, rest) = span isIdentifierChar cs
                      identifier = c:ending
                  in (lineno, identifier):scan lineno rest
    | otherwise = scan' lineno s

-- Scanning with look-ahead 1 didn't find anything so,
-- try with look-ahead 2. Switch back to look-ahead 1 after.
scan' :: Int -> String -> [Token]
scan' lineno s@(c:c':cs)
    | cc' == "--" = let rest = dropWhile (/='\n') cs
                    in scan lineno rest
    | cc' `elem` twoCharOps = (lineno, cc'):scan lineno cs
    | otherwise = (lineno, [c]):scan lineno c'cs where
      cc' = [c, c']
      c'cs = c':cs
scan' lineno s@(c:cs) = (lineno, [c]):scan lineno cs

-- The first character of an identifier must be a letter. The
-- rest can be letters, numbers or _
isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_'

-- Operators consisting of 2 characters
twoCharOps = filter ((==2) . length) $  map fst binaryOps

--syntax :: [Token] -> Program

--coreParse :: [String] -> Program
--coreParse = coreSyntax . coreLex


