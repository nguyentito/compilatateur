import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Char
import Data.List
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process (system)
import System.Posix.IO
import System.Posix.Process
import Text.Printf


-- Adapted from XMonad.Util.Run
spawnPipe :: String -> [String] -> IO Handle
spawnPipe executable arguments = do
  (rd, wr) <- createPipe
  setFdOption rd CloseOnExec True
  h <- fdToHandle rd
  hSetBuffering h LineBuffering
  pid <- forkProcess $ do
    _ <- dupTo wr stdOutput
    executeFile executable True arguments Nothing
  closeFd wr
  return h


runTest path = do
  putStrLn $ "Testing " ++ path
  exitCode <- system (printf "%s %s" "./minic" path)
  case exitCode of
    ExitFailure _ -> putStrLn "[[Compilation error]]\n" >> return False
    ExitSuccess   -> do
      h <- spawnPipe "spim" ["-file", "a.out.s"]
      -- get rid of "Loaded: /usr/share/spim/exceptions.s"
      result <- unlines . tail . lines <$> hGetContents h
      expected <- readFile $ init (init path) ++ ".out"
      if result == expected
        then True  <$ putStrLn "[[Success]]"
        else False <$ putStrLn "[[Execution failure]]"
      
main = do
  putStrLn $ "Execution tests"
  fileList <- sort . filter relevant <$> getDirectoryContents dir
  let pathList = map (dir ++) fileList
  results <- mapM runTest pathList
  let numSuccesses = length $ filter id results
      numFailures  = length $ filter not results
  putStrLn $ "Summary for " ++ dir ++ ":"
  putStrLn $ show numSuccesses ++ " successes, " ++ show numFailures ++ " failures."
  mapM_ (putStrLn . uncurry (++) . first oko) $ zip results fileList
    where relevant = (".c" `isSuffixOf`)
          dir = "tests/exec/"
          oko b | b           = "[OK] "
                | otherwise   = "[KO] "

