module Scan ( withFiles
            )
where

import Conf

import Data.List
import Control.Monad
import System.Directory
import System.FilePath.Posix

withFiles :: Conf -> (FilePath -> IO ()) -> (FilePath -> IO ()) -> IO ()
withFiles conf handleDirectory handleFile = recurse "."
    where
        recurse :: FilePath -> IO ()
        recurse path = do
            names <- sort <$> listDirectory path
            forM_ names $ \name ->
                unless (isIgnored conf name) $ do
                    let fullName = makeRelative "." (path </> name)
                    isFile <- doesFileExist fullName
                    isDir <- doesDirectoryExist fullName
                    case (isFile, isDir) of
                        (True, _) -> handleFile fullName
                        (_, True) -> handleDirectory fullName >> recurse fullName
                        (False, False) -> error $ "Can not push " ++ fullName

