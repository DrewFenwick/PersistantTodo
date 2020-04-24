module Task where

data Status 
  = Pending
  | Completed

data Task = Task 
  { title :: String
  , status :: Status
  }