import           System.IO       (IOMode (ReadMode, WriteMode), hClose,
                                  hGetContents, hPutStr, hSetEncoding, openFile,
                                  utf8)
import           System.IO.Error (tryIOError)
import           Text.Read

type Item = String
type Items = [Item]

data Command = Quit | DisplayItems | AddItem Item | Help | Remove Int | Save

addItem :: Item -> Items -> Items
addItem = (:)

displayItems :: Items -> String
displayItems = unlines . displayedItemList . reverse where
    displayedItemList = zipWith (\index item -> show index ++ " - " ++ item) [1..]

removeItem :: Int -> Items -> Either String Items
removeItem index items = impl (length items - index) items where
    impl 0 (_:rest)    = Right rest
    impl _ []          = Left "No item to remove"
    impl n (item:rest) = case impl (n-1) rest of
                            Right newItems -> Right (item:newItems)
                            Left errMsg    -> Left errMsg
save :: Items -> IO Items
save items = do
    putStrLn "File to save: "
    fileName <- getLine
    file <- tryIOError $ openFile fileName WriteMode
    case file of
        Left _ -> putStrLn "Error opening the file!" >> return items
        Right fileO -> do
            hSetEncoding fileO utf8
            result <- tryIOError . hPutStr fileO $ unlines items
            case result of
                Left _  -> putStrLn "Error writting to the file!"
                Right _ -> hClose fileO
            return items

parseCommand :: String -> Either String Command
parseCommand line = case words line of
    ["save"]           -> Right Save
    ["quit"]           -> Right Quit
    ["items"]          -> Right DisplayItems
    "add" : item       -> Right . AddItem $ unwords item
    ["help"]           -> Right Help
    "remove" : [num]   -> case readMaybe num :: Maybe Int of
                            Just x  -> Right $ Remove x
                            Nothing -> Left "Invalid index"
    _                  -> Left "Wrong command."


interactWithUser :: Items -> IO ()
interactWithUser items = putStr "> " >> getLine >>= executeCommand  items . parseCommand

executeCommand :: Items -> Either String Command -> IO ()
executeCommand items (Right Save)           = save items >> interactWithUser items
executeCommand items (Right DisplayItems)   = foldMap putStrLn ["The list of items is:", displayItems items] >> interactWithUser items
executeCommand items (Right (AddItem item)) = putStrLn "Item added" >> interactWithUser (addItem item items)
executeCommand items (Right (Remove index)) = case removeItem index items of
    Right newItems -> putStrLn "Item removed" >> interactWithUser newItems
    Left errorMsg  -> putStrLn errorMsg >> interactWithUser items

executeCommand items (Right Quit)  = putStrLn "Bye!"
executeCommand items (Right Help)  = putStrLn "Commands: help, quit, items, add <item to add>, remove <index>" >> interactWithUser items
executeCommand items (Left errMsg) = putStrLn ("Error: " ++ errMsg ++ "\n") >> interactWithUser items


main :: IO ()
main = putStrLn "TODO app" >> interactWithUser []
