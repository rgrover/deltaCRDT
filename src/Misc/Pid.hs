module Misc.Pid (Pid(..)) where

-- type for ProcessID
newtype Pid =
    P Int
    deriving (Eq, Ord, Show)
