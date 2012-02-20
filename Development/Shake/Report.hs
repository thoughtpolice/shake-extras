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
       ( buildReport -- :: FilePath -> FilePath -> IO ()
       ) where
import Data.ByteString as B (ByteString, readFile)
import Data.ByteString.Lazy as BL
import Text.Hastache as H
import Text.Hastache.Context as H

--import System.Directory (removeFile)
import System.IO.Unsafe (unsafePerformIO)
import System.FilePath
import Paths_shake_extras (getDataFileName)

data ReportCtx
  = C { ctxBootstrapCSS  :: B.ByteString
      , ctxBootstrapRCSS :: B.ByteString
      , ctxBootstrapJS   :: B.ByteString
      , ctxJquery        :: B.ByteString
      , ctxFlot          :: B.ByteString
      , ctxShakeJS       :: B.ByteString
      , ctxShakeDump     :: B.ByteString
      }

-- | Path to extras directory so we can find the report template, etc.
extrasDir :: FilePath
extrasDir = unsafePerformIO $ getDataFileName "extras"
{-# NOINLINE extrasDir #-}

-- | Takes a filepath pointing to the Shake \".js\" profiling dump.
-- Second path indicates the output file to write.
buildReport :: FilePath -> FilePath -> IO ()
buildReport jsfile out = do
  bcss    <- B.readFile $ bdir </> "css" </> "bootstrap.min.css"
  brcss   <- B.readFile $ bdir </> "css" </> "bootstrap-responsive.min.css"
  bjs     <- B.readFile $ bdir </> "js" </> "bootstrap.min.js"
  jquery  <- B.readFile $ extrasDir </> "jquery-1.6.4.min.js"
  flot    <- B.readFile $ extrasDir </> "jquery.flot.min.js"
  shakejs <- B.readFile $ extrasDir </> "shake.js"
  dump <- B.readFile jsfile

  let conf = H.defaultConfig { muEscapeFunc = H.emptyEscape }
  let ctx  = C bcss brcss bjs jquery flot shakejs dump
  BL.writeFile out =<< (hastacheFile conf (extrasDir </> "report.html") (context ctx))
--  removeFile jsfile
  return ()
 where bdir = extrasDir </> "bootstrap"

context :: Monad m => ReportCtx -> MuContext m
context ctx = H.mkStrContext $ \name -> case name of
  "bootstrapcss"  -> MuVariable $ ctxBootstrapCSS ctx
  "bootstraprcss" -> MuVariable $ ctxBootstrapRCSS ctx
  "bootstrapjs"   -> MuVariable $ ctxBootstrapJS ctx
  "jquery"        -> MuVariable $ ctxJquery ctx
  "flot"          -> MuVariable $ ctxFlot ctx
  "shakejs"       -> MuVariable $ ctxShakeJS ctx
  "shakedump"     -> MuVariable $ ctxShakeDump ctx
  _               -> MuNothing
