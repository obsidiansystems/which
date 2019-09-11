{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module System.Which (which, staticWhich) where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Language.Haskell.TH (Exp, Q, reportError, runIO)
import System.Directory (findExecutablesInDirectories)
import System.Environment (lookupEnv)
import System.FilePath (searchPathSeparator)

-- | Determine which executable would run if the given path were
--   executed, or return Nothing if a suitable executable cannot be
--   found
which :: FilePath -> IO (Maybe FilePath)
which f = do
  path <- runMaybeT $ MaybeT (lookupEnv "HOST_PATH") <|> MaybeT (lookupEnv "PATH")
  case path of
    Just path' -> fmap listToMaybe $
      findExecutablesInDirectories (fmap T.unpack $
        T.split (== searchPathSeparator) $ T.pack path') f
    Nothing -> pure Nothing

-- | Run `which` at compile time, and substitute the full path to the executable.
--
-- This is useful in NixOS to ensure that the resulting executable
-- contains the dependency in its closure and that it refers to the
-- same version at run time as at compile time
staticWhich :: FilePath -> Q Exp
staticWhich f = do
  mf' <- runIO $ which f
  case mf' of
    Nothing -> compileError $ "Could not find executable for " <> show f
    Just f'
      | "/nix/store/" `isPrefixOf` f' -> [| f' |]
      | otherwise -> compileError $
        "Path to executable " <> show f <> " was found in " <> show f'
        <> " which is not in /nix/store. Be sure to add the relevant package to 'backendTools' in default.nix."

  where
    compileError msg' = do
      let msg = "staticWhich: " <> msg'
      reportError msg
      [| error msg |]
