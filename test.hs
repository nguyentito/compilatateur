import Control.Applicative
import Data.List
import System.Directory
import System.Exit
import System.Process
import Text.Printf

execTest cmd path = do
  exitCode <- System.Process.system (printf "%s %s" cmd path)
  putStrLn $ case exitCode of
    ExitSuccess -> "Success"
    ExitFailure 1 -> "Failure"
    _ -> "Execution terminated abnormally"
    
runTests name dir testCmd = do
  putStrLn name
  fileList <- sort <$> getDirectoryContents ("tests/" ++ dir)
  mapM_ runTest fileList
  putStrLn ""
    where runTest filename
            | ".c" `isSuffixOf` filename = do
                let path = printf "tests/%s/%s" dir filename
                putStrLn path
                -- todo : handle cases where the path is invalid
                execTest testCmd path
                putStrLn ""
            | otherwise = pure ()
  
  
main = do
  runTests "Tests positifs de syntaxe"  "syntax/good" "./minic -parse-only";
  runTests "Tests négatifs de syntaxe"  "syntax/bad"  "./minic -parse-only";
  runTests "Tests positifs de typage"   "typing/good" "./minic -type-only";
  runTests "Tests négatifs de typage"   "typing/bad"  "./minic -type-only";
  runTests "Tests positifs d'exécution" "exec"        "./minic";
  runTests "Tests négatifs d'exécution" "exec-fail"   "./minic"
