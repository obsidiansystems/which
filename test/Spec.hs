import Control.Exception
import Data.List
import Data.Monoid ((<>))
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Posix.Temp
import System.Which

shouldEqual :: Eq a => Show a => a -> a -> IO ()
shouldEqual a b | a /= b = do
  putStrLn $ show a <> " does not equal " <> show b
  exitFailure
shouldEqual a b = putStrLn $ show a <> " == " <> show b

makeExecutable :: FilePath -> IO ()
makeExecutable f = setPermissions f . setOwnerExecutable True =<< getPermissions f

setup :: IO (FilePath, FilePath, FilePath)
setup = do
  bin <- mkdtemp "bin"
  bin2 <- mkdtemp "bin2"
  bin3 <- mkdtemp "bin3"

  appendFile (bin </> "hello") "hello"
  makeExecutable $ bin </> "hello"
  appendFile (bin2 </> "hello") "hello2"
  makeExecutable $ bin2 </> "hello"
  appendFile (bin2 </> "hello2") "hello2"
  makeExecutable $ bin2 </> "hello2"
  appendFile (bin3 </> "hello") "hello"
  makeExecutable $ bin3 </> "hello"
  appendFile (bin3 </> "hello2") "hello2"

  appendFile (bin3 </> "hello3") "hello3"
  makeExecutable $ bin3 </> "hello3"

  pure (bin, bin2, bin3)

cleanup :: (FilePath, FilePath, FilePath) -> IO ()
cleanup (bin, bin2, bin3) = do
  removeDirectoryRecursive bin
  removeDirectoryRecursive bin2
  removeDirectoryRecursive bin3

main :: IO ()
main = bracket setup cleanup $ \(bin, bin2, bin3) -> do
  unsetEnv "PATH"
  unsetEnv "HOST_PATH"

  fp <- which "hello"

  shouldEqual fp Nothing

  setEnv "PATH" $ concat $ intersperse ":" [bin, bin2, bin3]
  setEnv "HOST_PATH" $ concat $ intersperse ":" [bin, bin2]

  fp1 <- which "hello"
  fp2 <- which "hello2"
  fp3 <- which "hello3"

  shouldEqual fp1 (Just $ bin </> "hello")
  shouldEqual fp2 (Just $ bin2 </> "hello2")
  shouldEqual fp3 Nothing

  unsetEnv "PATH"

  fp4 <- which "hello"
  fp5 <- which "hello2"
  fp6 <- which "hello3"

  shouldEqual fp4 (Just $ bin </> "hello")
  shouldEqual fp5 (Just $ bin2 </> "hello2")
  shouldEqual fp6 Nothing

  unsetEnv "HOST_PATH"
  setEnv "PATH" $ concat $ intersperse ":" [bin3, bin2, bin]

  fp7 <- which "hello"
  fp8 <- which "hello2"
  fp9 <- which "hello3"

  shouldEqual fp7 (Just $ bin3 </> "hello")
  shouldEqual fp8 (Just $ bin2 </> "hello2")
  shouldEqual fp9 (Just $ bin3 </> "hello3")

  pure ()
