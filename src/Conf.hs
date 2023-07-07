{-# LANGUAGE TupleSections #-}

module Conf ( Conf
            , getConf
            , withFTP'
            , isIgnored
            )
where

import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.List.Extra
import Network.FTP.Client
import System.Directory
import System.Exit
import System.FilePath.Posix
import Text.Pretty.Simple

confFile :: FilePath
confFile = ".push"

data Conf = Conf { servers :: [ServerConf]
                 , ignore :: [FilePath]
                 , keep :: [FilePath]
                 }
            deriving (Show, Read)

data ServerConf = ServerConf { server :: String
                             , proto :: Proto
                             , user :: String
                             , password :: String
                             , root :: FilePath
                             }
                  deriving (Show, Read)

data Proto = FTP Int | SFTP Int
           deriving (Show, Read)

getConf :: IO Conf
getConf = do
    (dir, conf) <- findConf =<< getCurrentDirectory
    setCurrentDirectory dir
    return conf

findConf :: FilePath -> IO (FilePath, Conf)
findConf path = do
    when (isDrive path) confFileNotFound
    let name = path </> confFile
    confFound <- doesFileExist name
    if confFound
        then (path,) . read <$> readFile name
        else findConf (takeDirectory path)

confFileNotFound :: IO a
confFileNotFound = do
    putStrLn $ confFile ++ " not found"
    putStrLn $ "Please create a " ++ confFile ++ " at the root of the directory to push containing:"
    pPrint $ Conf { servers = [ ServerConf { server = "remote FTP server"
                                           , proto = FTP 21
                                           , user = "user"
                                           , password = "password"
                                           , root = "root directory on the remove server"
                                           }
                              ]
                  , ignore = ["list of files or directories to ignore"]
                  , keep = ["list of files or directories to keep"]
                  }
    exitFailure

isIgnored :: Conf -> FilePath -> Bool
isIgnored conf name = (isHidden || isTmp || inBlackList) && not inWhiteList
    where
        isHidden = "." `isPrefixOf` name
        isTmp = "~" `isPrefixOf` name || ".swp" `isSuffixOf` name
        inBlackList = name `elem` ignore conf
        inWhiteList = name `elem` keep conf

withFTP' :: Conf -> (String -> Handle -> IO ()) -> IO ()
withFTP' conf action = do
    forM_ (servers conf) $ \serverConf -> do
        let session = case proto serverConf of
                FTP port -> withFTP (server serverConf) port
                SFTP port -> withFTPS (server serverConf) port
        session $ \ftp welcome -> do
            print welcome
            check =<< login ftp (user serverConf) (password serverConf)
            check =<< cwd ftp (root serverConf)
            action (server serverConf) ftp

check :: FTPResponse -> IO ()
check FTPResponse { frStatus = Success } = return ()
check FTPResponse { frMessage = SingleLine s } = error $ "FTP error: " ++ B.unpack s
check FTPResponse { frMessage = MultiLine ss } = error $ "FTP error: \n" ++ unlines (map B.unpack ss)
