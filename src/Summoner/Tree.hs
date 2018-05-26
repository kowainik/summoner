module Summoner.Tree
       ( TreeFs (..)
       , traverseTree
       ) where

import System.Directory (createDirectoryIfMissing, withCurrentDirectory)

-- | Describes simple structure of filesystem tree.
data TreeFs
      -- | Name of directory (relative) and its containing entries
    = Dir FilePath [TreeFs]
      -- | File name (relative) and file content
    | File FilePath Text

-- | Walks through directory tree and write file contents, creating all
-- intermediate directories.
traverseTree :: TreeFs -> IO ()
traverseTree (Dir name children) = do
    createDirectoryIfMissing False name
    withCurrentDirectory name $ for_ children traverseTree
traverseTree (File name content) = writeFile name content
