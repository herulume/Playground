dialogo :: String -> IO String
dialogo s = putStr s >> getLine

questionario :: [String] -> IO [String]
questionario [] = return [];
questionario (q:qs) = dialogo q >>= \x -> questionario qs >>= (\y -> return (x:y))

