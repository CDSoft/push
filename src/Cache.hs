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
type Cache = MVar (M.Map FilePath Item)

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
writeCache cacheMVar item = do
    cache <- takeMVar cacheMVar
    let cache' = M.insert (itemName item) item cache
    writeFile cacheFile (show cache')
    putMVar cacheMVar cache'

removeCache :: Cache -> String -> IO ()
removeCache cacheMVar name = do
    cache <- takeMVar cacheMVar
    let cache' = M.delete name cache
    writeFile cacheFile (show cache')
    putMVar cacheMVar cache'

