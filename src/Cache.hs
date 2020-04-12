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

data Item = Dir FilePath
          | File FilePath Integer UTCTime
          deriving (Show, Read, Eq)
type CacheMap = M.Map FilePath Item
type Cache = MVar CacheMap

itemName :: Item -> FilePath
itemName (Dir name) = name
itemName (File name _ _) = name

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

lookupCache :: FilePath -> Cache -> IO (Maybe Item)
lookupCache name cacheMVar =
    M.lookup name <$> readMVar cacheMVar

writeCache :: Cache -> Item -> IO ()
writeCache cacheMVar item =
    withCache cacheMVar $ M.insert (itemName item) item

removeCache :: Cache -> String -> IO ()
removeCache cacheMVar name =
    withCache cacheMVar $ M.delete name

withCache :: Cache -> (CacheMap -> CacheMap) -> IO ()
withCache cacheMVar action = do
    cache <- action <$> takeMVar cacheMVar
    writeFile cacheFile (show cache)
    putMVar cacheMVar cache
