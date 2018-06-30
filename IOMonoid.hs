-- haskellforall.com/2018/02/the-wizard-monoid.html
import Data.Monoid hiding ((<>))
import qualified System.Directory

prompt :: String -> IO (IO ())
prompt attribute = do
    putStrLn("What is your " ++ attribute ++ "?")
    x <- getLine
    return (putStrLn ("Your " ++ attribute ++ " is: " ++ x))

runWizard :: IO (IO a) -> IO a
runWizard request = do
    respond <- request
    respond


main :: IO ()
main = runWizard $ foldMap prompt ["name", "age", "number"]


promptFiles :: FilePath -> IO (IO ())
promptFiles file = do 
    putStrLn ("Would you like to delete " ++ file ++ "?")
    response <- getLine
    case response of
        "y" -> return (do
            putStrLn ("Removing " ++ file)
            System.Directory.removeFile file)
        _ -> return (return ()) 


mainFiles :: IO ()
mainFiles = do
    files <- System.Directory.listDirectory "."
    runWizard $ foldMap promptFiles files
