module Main where

import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.Map as M
import Analyse
import Parser
import Lexer
import Compile
import qualified Turtle as T

import Prelude

import System.Environment
import System.Console.GetOpt

import Text.Printf

-------------Options------------------
data Options = Options {
  optHelp :: Bool,
  optVersion :: Bool,
  optDebug :: Bool
  } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options {
  optHelp = False,
  optVersion = False,
  optDebug = False
  }

options :: [OptDescr (Options -> Options)]
options = [ Option ['h'] ["help"]
            (NoArg (\opts -> opts {optHelp = True}))
            "Output Help"
          , Option ['v'] ["version"]
            (NoArg (\opts -> opts {optVersion = True}))
            "Print Version information"
          , Option ['d'] ["debug"]
            (NoArg (\opts -> opts {optDebug = True}))
            "Print string representation of output Turtle assembly"
          ]

turtleOptions :: [String] -> Either String (Options, Maybe String)
turtleOptions argv = case getOpt Permute options argv of
  (o, [n],   []) -> Right (foldl (flip id) defaultOptions o, Just n)
  (o, _,     []) -> Right (foldl (flip id) defaultOptions o, Nothing)
  (_, _,   errs) -> Left $ concat errs ++ usageInfo header options
  where
    header = "Usage: TurtleCompiler [OPTION...] filename"

getProgram :: Maybe String -> IO String
getProgram file = case file of
  Nothing  -> getContents
  Just str -> readFile str

------------------Main--------------------
main :: IO ()
main = do
  argv <- getArgs
  case turtleOptions argv of
    Left e -> ioError $ userError e
    Right (opts, fname)
      | optHelp opts -> putStrLn $ usageInfo "Usage: TurtleCompiler [OPTION...] filename" options
      | optVersion opts -> putStrLn "Turtle Compiler version 1.0"
      | otherwise -> do
      inStr <- getProgram fname --return testStr
      let parseTree = parse (alexScanTokens inStr)
          (err, ctx) = runState (runExceptT $ checkProgram parseTree) M.empty
      case err of
        Right ()
          | optDebug opts -> T.output $ T.runST $ do
              (CompType ix mem _ _) <- execStateT (compileProgram parseTree) (CompType 0 undefined ctx [])
              vec <- T.freezeProg mem
              return (ix, vec)
          | otherwise -> T.output $ T.runST $ do
              (CompType ix mem _ _) <- execStateT (compileProgram parseTree) (CompType 0 undefined ctx [])
              vec <- T.freezeMem mem
              return (ix, vec)
        Left (code, str)   -> putStrLn $ printf (show code) str
