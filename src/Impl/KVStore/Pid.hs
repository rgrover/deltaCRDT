module Impl.KVStore.Pid (Pid(..)) where

-- type for ProcessID
newtype Pid =
    P Word
    deriving (Eq, Ord, Show)
