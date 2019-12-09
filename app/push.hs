module Main
where

import Cache
import Conf
import Push
import Scan

main :: IO ()
main = do
    conf <- getConf
    cache <- getCache
    withFTP' conf $ \ftp -> do
        withFilesInCache cache (cleanItem ftp cache)
        withFiles conf (pushDir ftp cache) (pushFile ftp cache)
