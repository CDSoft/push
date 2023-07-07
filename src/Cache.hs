module Cache ( Item(..)
             , itemName
             , Cache
             , getCache
             , writeCache
             , removeCache
             , lookupCache
             , withFilesInCache
             )
where

import Control.Concurrent.MVar
import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Time.Clock
import System.Directory

cacheFile :: FilePath
cacheFile = ".cache"

data Item = Dir String FilePath
          | File String FilePath Integer UTCTime
          deriving (Show, Read, Eq)
type CacheMap = M.Map (String, FilePath) Item
type Cache = MVar CacheMap

itemName :: Item -> (String, FilePath)
itemName (Dir server name) = (server, name)
itemName (File server name _ _) = (server, name)

getCache :: IO Cache
getCache = do
    cacheFound <- doesFileExist cacheFile
    newMVar =<< if cacheFound
        then read <$> readFile cacheFile
        else return M.empty

withFilesInCache :: Cache -> (Item -> IO ()) -> IO ()
withFilesInCache cacheMVar handleItem = do
    cache <- readMVar cacheMVar
    let items = sortOn itemName (M.elems cache)
    forM_ (reverse items) handleItem

lookupCache :: String -> FilePath -> Cache -> IO (Maybe Item)
lookupCache server name cacheMVar =
    M.lookup (server, name) <$> readMVar cacheMVar

writeCache :: Cache -> Item -> IO ()
writeCache cacheMVar item =
    withCache cacheMVar $ M.insert (itemName item) item

removeCache :: Cache -> String -> FilePath -> IO ()
removeCache cacheMVar server name =
    withCache cacheMVar $ M.delete (server, name)

withCache :: Cache -> (CacheMap -> CacheMap) -> IO ()
withCache cacheMVar action = do
    cache <- action <$> takeMVar cacheMVar
    writeFile cacheFile (show cache)
    putMVar cacheMVar cache
