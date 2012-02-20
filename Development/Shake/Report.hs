-- |
-- Module      : Development.Shake.Report
-- Copyright   : (c) Austin Seipp 2012
-- License     : BSD3
-- 
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : GHC probably
-- 
-- This module provides a convenient
-- cmdargs style interface for controlling
-- shake build options.
-- 
module Development.Shake.Report
       ( buildReportTemplate -- :: FilePath -> FilePath -> IO ()
       ) where
import Data.ByteString as B (ByteString, readFile)
import Data.ByteString.Lazy as BL
import Text.Hastache as H
import Text.Hastache.Context as H

import System.Directory (removeFile)
import System.IO.Unsafe (unsafePerformIO)
import System.FilePath
import Paths_shake_extras (getDataFileName)

-- | Path to extras directory so we can find the report template, etc.
extrasDir :: FilePath
extrasDir = unsafePerformIO $ getDataFileName "extras"
{-# NOINLINE extrasDir #-}

-- | Takes a filepath pointing to the Shake \".js\" profiling dump.
-- Second path indicates the output file to write.
buildReportTemplate :: FilePath -> FilePath -> IO ()
buildReportTemplate jsfile out = do
  js       <- B.readFile jsfile
  let conf = H.defaultConfig { muEscapeFunc = H.emptyEscape }
  BL.writeFile out =<< (hastacheFile conf (extrasDir </> "report.html") (context js))
  removeFile jsfile
  return ()

context :: Monad m => B.ByteString -> MuContext m
context shakejs = H.mkStrContext $ \name -> case name of
  "bootstrapcss"  -> MuVariable $ bdir </> "css" </> "bootstrap.min.css"
  "bootstrapjs"   -> MuVariable $ bdir </> "js" </> "bootstrap.min.js"
  "jquery"        -> MuVariable $ extrasDir </> "jquery-1.7.1.min.js"
  "shakedump"     -> MuVariable $ shakejs
  _               -> MuNothing
 where bdir = extrasDir </> "bootstrap"
