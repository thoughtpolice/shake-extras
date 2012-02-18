-- |
-- Module      : Development.Shake.CLI
-- Copyright   : (c) Austin Seipp 2012
-- License     : BSD3
-- 
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : ???
-- 
module Development.Shake.CLI
       ( options --
       , shake'  -- 
       ) where
import System.Console.CmdArgs as CA

import Development.Shake as Shake

options :: ShakeOptions
options 
  = ShakeOptions { shakeFiles     = ".shake" &= help "shake journal/db path"
                 , shakeThreads   = 1        &= help "threads to use for build" &= name "j" &= typ "NUM"
                 , shakeVersion   = 1        &= help "build system ver; increment to rebuild"
                 , shakeStaunch   = False    &= help "keep going after error"  &= name "k"
                 , shakeDump      = False    &= help "dump profiling report"   &= name "prof"
                 , shakeLint      = False    &= help "run build system linter" &= name "lint"
                 , shakeVerbosity = Shake.Normal &= ignore
                 } &= verbosity

shake' :: Rules () -> IO ()
shake' r = do
  x <- cmdArgs options
  v <- CA.getVerbosity
  case v of
    CA.Normal -> shake x r
    CA.Quiet  -> shake (x { shakeVerbosity = Shake.Quiet }) r 
    CA.Loud   -> shake (x  { shakeVerbosity = Shake.Loud }) r
