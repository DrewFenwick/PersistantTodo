module Main where

import Options.Applicative
import PersistentTodo.Command

main :: IO ()
main = do
  performCommand =<< execParser opts

performCommand _ = undefined

opts :: ParserInfo Command
opts = undefined

command :: Parser Command
command = undefined