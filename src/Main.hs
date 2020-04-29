module Main where

import           Data.Foldable                  ( fold )
import           Options.Applicative
import           PersistentTodo.Command
import           PersistentTodo.Task (Title(..))

main :: IO ()
main = performCommand =<< execParser opts

performCommand :: Command -> IO ()
performCommand = undefined

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
parseAdd = Add <$> argument (Title <$> str) (metavar "TITLE")

parseComplete :: Parser Command
parseComplete = Complete <$> getPos

parseMove :: Parser Command
parseMove = Move <$> getPos <*> getPos

parseRemove :: Parser Command
parseRemove = Remove <$> getPos

getPos :: Parser Place
getPos = argument (Place <$> auto) (metavar "POSITION")
