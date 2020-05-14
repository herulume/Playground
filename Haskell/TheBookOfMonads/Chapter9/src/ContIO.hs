module ContIO where

import System.IO
import Control.Monad.Managed

-- IO r -> (r -> IO a) -> (r -> IO ()) -> IO a
-- acq      use            release

-- withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
-- withFile fp mode = bracket (openFile fp mode) hClose

doWork :: Handle -> Handle -> IO ()
doWork inH outH = hGetContents inH >>= hPutStr outH

readWrite :: FilePath -> FilePath -> IO ()
readWrite inFile outFile =
    withFile inFile ReadMode $ \inHandle ->
        withFile outFile WriteMode $ \outHandle ->
            doWork inHandle outHandle

readWriteManaged :: FilePath -> FilePath -> IO ()
readWriteManaged inFile outFile = runManaged $ do
    inHandle <- managed (withFile inFile ReadMode)
    outHandle <- managed (withFile outFile WriteMode)
    liftIO $ doWork inHandle outHandle

