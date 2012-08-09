{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}
-- |
-- Module      : Development.Shake.Imports
-- Copyright   : (c) Soenke Hahn 2012
-- License     : BSD3
--
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : GHC probably
--
-- Module to search imports. Imports are files that are needed for
-- compilation of a source code file.  (This module is experimental.)
--
module Development.Shake.Imports (
    -- * declaring rules for imports
    importsRule,
    importsDefaultHaskell,
    importsDefaultCpp,
    -- * querying imports
    directImports,
    transitiveImports,
  ) where

import Data.List
import Data.Char
import Data.Maybe

import Control.Applicative ((<$>))
import Control.Arrow

import Development.Shake
import Development.Shake.FilePath


-- * querying imports of a file

-- | Searches for imports in a file. Before compiling, you still have to 'need' the imports.
directImports :: FilePath -> Action [FilePath]
directImports file =
    lines <$> readFile' (file <.> "directImports")

-- | Searches for transitive imports in a file. This might be useful for linking.
transitiveImports :: FilePath -> Action [FilePath]
transitiveImports file =
    lines <$> readFile' (file <.> "transitiveImports")


-- * declaring rules

-- | @importsRule p searchImports@ registers a rule for how to look up imports
-- for files matching p.
importsRule :: (FilePath -> Bool)
    -> (FilePath -> Action [FilePath])
    -> Rules ()
importsRule ruleApplies getDirectDependencies = do

    let isDirectImportKey file =
            ("//*.directImports" ?== file) &&
            ruleApplies (dropExtension file)

    isDirectImportKey ?> \ importsFile -> do
        let sourceFile = dropExtension importsFile
        directImports' <- getDirectDependencies sourceFile
        writeFileChanged importsFile (unlines directImports')

    let isTransitiveImportKey file =
            ("//*.transitiveImports" ?== file) &&
            ruleApplies (dropExtension file)

    isTransitiveImportKey ?> \ transitiveImportsFile -> do
        let directImportsFile = replaceExtension transitiveImportsFile ".directImports"
        directImports' :: [FilePath] <- readFileLines directImportsFile
        transitiveImports' <- concat <$> mapM readFileLines
                            (map (<.> ".transitiveImports") directImports')
        writeFileChanged transitiveImportsFile
            (unlines $ nub $ directImports' ++ transitiveImports')

    return ()

-- * format specific functions

-- ** haskell source files

-- | Registers a defaultRule for imports in haskell source files (\".hs\").
-- Only returns the imported files that are found
-- on disk in one of the directories specified by the first argument.
importsDefaultHaskell :: [FilePath] -> Rules ()
importsDefaultHaskell sourceDirs =
    importsRule ("//*.hs" ?==) (getHaskellDependencies sourceDirs)
  where
    getHaskellDependencies :: [FilePath] -> FilePath -> Action [FilePath]
    getHaskellDependencies source file =
        readFile' file >>=
        return . hsImports >>=
        return . map moduleToFile >>=
        mapM (searchInPaths source) >>=
        return . catMaybes

    hsImports :: String -> [String]
    hsImports xs = [ takeWhile (\z -> isAlphaNum z || z `elem` "._") $ dropWhile (not . isUpper) x
                   | x <- lines xs, "import " `isPrefixOf` x]

    moduleToFile :: String -> FilePath
    moduleToFile =
        map (\ c -> if c == '.' then '/' else c) >>>
        (<.> "hs")


-- ** c++ source files

-- | Registers a defaultRule for imports in C++ source files (\".cpp\").
-- Only returns the imported files that are found
-- on disk in one of the directories specified by the first argument.
importsDefaultCpp :: [FilePath] -> Rules ()
importsDefaultCpp sourceDirs =
    importsRule ("//*.cpp" ?==) getDependencies
  where
    getDependencies file =
        readFile' file >>=
        return . extractLocalIncludes >>=
        mapM (searchInPaths sourceDirs) >>=
        return . catMaybes

    extractLocalIncludes :: String -> [FilePath]
    extractLocalIncludes =
        lines >>> map localInclude >>> catMaybes

    localInclude :: String -> Maybe FilePath
    localInclude line |
        ("#include" : quotedFile : []) <- words line,
        "\"" `isPrefixOf` quotedFile,
        "\"" `isSuffixOf` quotedFile
        = Just $ tail $ init $ quotedFile
    localInclude _ = Nothing


-- * utils

searchInPaths :: [FilePath] -> FilePath -> Action (Maybe FilePath)
searchInPaths (path : r) file = do
    exists <- doesFileExist (path </> file)
    if exists
        then return $ Just (path </> file)
        else searchInPaths r file
searchInPaths [] _ = return Nothing
