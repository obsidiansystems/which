{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module System.Which (which, staticWhich) where

import Control.Applicative
import Control.Monad (foldM)
import Control.Monad.Trans.Maybe
import Data.Bool (bool)
import Data.List (isPrefixOf)
import qualified Data.Text as T
import Data.Monoid ((<>))
import Language.Haskell.TH (Exp, Q, reportError, runIO)
import System.Directory (executable, getPermissions)
import System.Environment (lookupEnv)
import System.FilePath (searchPathSeparator)
import System.FilePath.Posix ((</>))
import System.IO.Error (catchIOError)

-- | Is this path executable?
isExecutable :: FilePath -> IO Bool
isExecutable f = catchIOError (fmap executable $ getPermissions f) (const $ pure False)

-- | Determine which executable would run if the given path were
--   executed, or return Nothing if a suitable executable cannot be
--   found
which :: FilePath -> IO (Maybe FilePath)
which f = do
  path <- (runMaybeT $ MaybeT (lookupEnv "HOST_PATH") <|> MaybeT (lookupEnv "PATH"))
  case path of
    Just path -> do
      fp <- lookupSearchPath f path
      case fp of
        Just fp' -> fmap (bool Nothing (Just fp')) $ isExecutable fp'
        Nothing -> pure Nothing
    Nothing -> pure Nothing

-- | Lookup a path inside of a given search path.
lookupSearchPath :: FilePath -> String -> IO (Maybe FilePath)
lookupSearchPath f path = findPath $ T.split (== searchPathSeparator) $ T.pack path
  where findPath [] = pure Nothing
        findPath (x:xs) =
          let fp = T.unpack x </> f
          in bool (findPath xs) (pure $ Just fp) =<< isExecutable fp

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
