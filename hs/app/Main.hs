module Main where

import Control.Monad (forM_)
import Parser
import System.Directory (getCurrentDirectory, listDirectory)
import System.FilePath (replaceExtension, takeExtension, (</>))

-- Run this function from `hs` directory.
main :: IO ()
main = do
  cwd <- getCurrentDirectory
  let dir = cwd </> "../test_files"
  files <- listDirectory dir
  let irFiles = filter (\f -> takeExtension f == ".ir") files
  forM_ irFiles $ \file -> do
    putStrLn $ "testing: " ++ file
    expected <- readFile (dir </> replaceExtension file ".ans")
    content <- readFile (dir </> file)
    let parsed = parse content
    let p = case parsed of
          Left _ -> error "parse error"
          Right res -> res
    if show p /= expected
      then
        error $
          "expected differs from actual: \n"
            ++ "expected:\n"
            ++ expected
            ++ "\nactual:\n"
            ++ show p
      else
        putStrLn "OK\n"
