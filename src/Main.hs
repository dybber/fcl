module Main where

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)

import Language.FCL (compileFromFiles, flagDebug)

data Command = ShowUsage | ParseOnly

data Config = Config {
    fclVerbose :: Bool,
    fclCommand :: Command,
    fclOutputFile :: String
  }

emptyConfig :: Config
emptyConfig = Config {
    fclVerbose = False,
    fclCommand = ParseOnly,
    fclOutputFile = "kernels.cl"
  }

optionDescriptions :: [OptDescr (Config -> Config)]
optionDescriptions = 
  [ Option "v" ["verbose"]    (NoArg (\opt -> opt {fclVerbose = True})) "Verbose output",
    Option "h" ["help"]       (NoArg (\opt -> opt {fclCommand = ShowUsage})) "Show help for this command",
    Option []  ["parse-only"] (NoArg (\opt -> opt {fclCommand = ParseOnly})) "Parse and print syntax tree",
    Option "o" ["output"]     (ReqArg (\out -> (\opt -> opt {fclOutputFile = out})) "FILE") "where to emit OpenCL kernels"
  ]

parseOptions :: IO ([String], Config)
parseOptions = do
  args <- getArgs
  case getOpt RequireOrder optionDescriptions args of
    (opts, files, []) -> return (files, foldl (.) id opts emptyConfig)
    (_, _, errs) -> do showErrMsg errs
                       showUsage
                       exitFailure

showErrMsg :: [String] -> IO ()
showErrMsg = mapM_ (hPutStrLn stderr . ("Error: " ++))

showUsage :: IO ()
showUsage = do
  prog <- getProgName
  let heading = "Help for FCL (" ++ prog ++ ")"
  putStr $ usageInfo heading optionDescriptions

main :: IO ()
main = do
 (filenames, cfg) <- parseOptions
 case fclCommand cfg of
   ShowUsage -> showUsage
   ParseOnly -> do
     let extensions = map extension filenames
     if all (== "fcl") extensions
       then do out <- compileFromFiles flagDebug filenames
               writeFile (fclOutputFile cfg) out
       else error "I can only handle .fcl files."

extension :: FilePath -> String
extension = reverse . takeWhile ((/=) '.') . reverse
