import System.Environment (getArgs)

interactWith :: (String -> String) -> FilePath -> FilePath-> IO ()
interactWith fun input output = readFile input >>= (\f -> writeFile output $ fun f)

main :: IO ()
main = getArgs >>= (\ lis -> case lis of 
                                [input, output] -> interactWith fixLines input output
                                _ -> putStrLn "error: Two arguments needed\n\t ./CleanLines inputFile outputFile"
                   )

splitLines :: String -> [String]
splitLines [] = []
splitLines cs = let (pre, suf) = break isLineTerminator cs
                in pre : case suf of
                             ('\r':'\n':rest) -> splitLines rest
                             ('\r':rest)      -> splitLines rest
                             ('\n':rest)      -> splitLines rest
                             _                -> []
isLineTerminator x = x == '\r' || x == '\n'

fixLines :: String -> String
fixLines = unlines . splitLines
