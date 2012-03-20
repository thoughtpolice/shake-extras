-- |
-- Module      : Development.Shake.CLI
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
module Development.Shake.CLI
       ( options       -- :: ShakeOptions
       , shakeWithArgs -- :: Rules () -> IO ()
       ) where
import Control.Monad (when)
import Development.Shake as Shake
import System.Console.CmdArgs as CA

-- | A 'ShakeOptions' data structure with 
-- annotations for CmdArgs already included.
options :: ShakeOptions
options 
  = ShakeOptions { shakeFiles     = ".shake" &= help "shake journal/db path"
                 , shakeThreads   = 1        &= help "threads to use for build" &= name "j" &= typ "NUM"
                 , shakeVersion   = 1        &= ignore
                 , shakeStaunch   = False    &= help "keep going after error"  &= name "k"
                 , shakeReport    = Nothing  &= help "dump profiling report" &= name "prof" &= typFile
                 , shakeLint      = False    &= help "run build system linter" &= name "lint"
                 , shakeVerbosity = Shake.Normal &= ignore
                 } &= verbosity

-- | Build a set of Shake rules, and let cmdargs take care of
-- parsing command line arguments that may influence the used
-- 'ShakeOptions'
shakeWithArgs :: Rules () -> IO ()
shakeWithArgs r = do
  x <- cmdArgs options
  v <- CA.getVerbosity
  case v of
    CA.Normal -> shake x r
    CA.Quiet  -> shake (x { shakeVerbosity = Shake.Quiet }) r 
    CA.Loud   -> shake (x { shakeVerbosity = Shake.Loud }) r
