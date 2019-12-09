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

pushDir :: Handle -> Cache -> FilePath -> IO ()
pushDir ftp cache name = do
    maybeItem <- lookupCache name cache
    let newDir = Dir name
    when (maybeItem /= Just newDir) $ do
        putStrLn $ "mkd " ++ name
        catch (void $ mkd ftp name)
              (\(e :: FTPException) -> hPrint stderr e)
        writeCache cache (Dir name)

pushFile :: Handle -> Cache -> FilePath -> IO ()
pushFile ftp cache name = do
    maybeItem <- lookupCache name cache
    newFile <- do
        newSize <- getFileSize name
        newDate <- getModificationTime name
        return $ File name newSize newDate
    when (maybeItem /= Just newFile) $ do
        putStrLn $ "stor " ++ name
        content <- B.readFile name
        stor ftp name content TI
        writeCache cache newFile

cleanItem :: Handle -> Cache -> Item -> IO ()
cleanItem ftp cache item = do
    let name = itemName item
    isFile <- doesFileExist name
    isDir <- doesDirectoryExist name
    actualItem <- case (isFile, isDir) of
        (True, _) -> do
            actualSize <- getFileSize name
            actualDate <- getModificationTime name
            return $ Just $ File name actualSize actualDate
        (_, True) -> return $ Just $ Dir name
        (False, False) -> return Nothing
    when (Just item /= actualItem) $ do
        case item of
            File{} -> when (isNothing actualItem) $ do
                putStrLn $ "dele " ++ name
                void $ dele ftp name
            Dir{} -> do
                putStrLn $ "rmd " ++ name
                void $ rmd ftp name
        removeCache cache name
