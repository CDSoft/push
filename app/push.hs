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
    withFTP' conf $ \server ftp -> do
        withFilesInCache cache (cleanItem ftp cache server)
        withFiles conf (pushDir ftp cache server) (pushFile ftp cache server)
