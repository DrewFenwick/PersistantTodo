module Main where

import           Options.Applicative
import           PersistentTodo.Command
import           PersistentTodo.Task            ( Title(..) )

main :: IO ()
main = performCommand =<< execParser opts

performCommand :: Command -> IO ()
performCommand = undefined

opts :: ParserInfo Command
opts = info parseCommand mempty

parseCommand :: Parser Command
parseCommand =
  subparser
    . foldMap (uncurry command)
    $ [ ("add"   , info parseAdd mempty)
      , ("done"  , info parseComplete mempty)
      , ("move"  , info parseMove mempty)
      , ("remove", info parseRemove mempty)
      , ("clean" , info (pure Clean) mempty)
      , ("wipe"  , info (pure Wipe) mempty)
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
