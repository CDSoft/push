{-# LANGUAGE ScopedTypeVariables #-}

module Push ( pushDir
            , pushFile
            , cleanItem
            )
where

import Cache

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Network.FTP.Client
import System.Directory
import System.IO (hPrint, stderr)

pushDir :: Handle -> Cache -> String -> FilePath -> IO ()
pushDir ftp cache server name = do
    maybeItem <- lookupCache server name cache
    let newDir = Dir server name
    when (maybeItem /= Just newDir) $ do
        putStrLn $ "["++server++"] mkd " ++ name
        catch (void $ mkd ftp name)
              (\(e :: FTPException) -> hPrint stderr e)
        writeCache cache newDir

pushFile :: Handle -> Cache -> String -> FilePath -> IO ()
pushFile ftp cache server name = do
    maybeItem <- lookupCache server name cache
    newFile <- do
        newSize <- getFileSize name
        newDate <- getModificationTime name
        return $ File server name newSize newDate
    when (maybeItem /= Just newFile) $ do
        putStrLn $ "["++server++"] stor " ++ name
        content <- B.readFile name
        stor ftp name content TI
        writeCache cache newFile

cleanItem :: Handle -> Cache -> String -> Item -> IO ()
cleanItem ftp cache server item = do
    let (server0, name) = itemName item
    isFile <- doesFileExist name
    isDir <- doesDirectoryExist name
    actualItem <- case (isFile, isDir) of
        (True, _) -> do
            actualSize <- getFileSize name
            actualDate <- getModificationTime name
            return $ Just $ File server0 name actualSize actualDate
        (_, True) -> return $ Just $ Dir server name
        (False, False) -> return Nothing
    when (Just item /= actualItem) $ do
        case item of
            File{} -> when (isNothing actualItem) $ do
                putStrLn $ "["++server++"] dele " ++ name
                void $ dele ftp name
            Dir{} -> do
                putStrLn $ "["++server++"] rmd " ++ name
                void $ rmd ftp name
        removeCache cache server name
