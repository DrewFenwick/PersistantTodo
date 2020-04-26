module Main where

import           Data.Foldable                  ( fold )
import           Options.Applicative
import           PersistentTodo.Command

main :: IO ()
main = performCommand =<< execParser opts

performCommand :: Command -> IO ()
performCommand = print

opts :: ParserInfo Command
opts = info parseCommand mempty

parseCommand :: Parser Command
parseCommand = subparser $ fold
  [ command "add"    (info parseAdd mempty)
  , command "done"   (info parseComplete mempty)
  , command "move"   (info parseMove mempty)
  , command "remove" (info parseRemove mempty)
  , command "clean"  (info (pure Clean) mempty)
  , command "wipe"   (info (pure Wipe) mempty)
  ]

parseAdd :: Parser Command
parseAdd = Add <$> argument str (metavar "TITLE")

parseComplete :: Parser Command
parseComplete = Complete <$> getPos

parseMove :: Parser Command
parseMove = Move <$> getPos <*> getPos

parseRemove :: Parser Command
parseRemove = Remove <$> getPos

getPos = argument auto (metavar "POSITION")
