{-# LANGUAGE ViewPatterns #-}

module Summoner.Tree
       ( TreeFs (..)
       , traverseTree
       , showTree
       ) where

import Universum

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

-- | Pretty shows the directory tree content.
showTree :: TreeFs -> Text
showTree = unlines . showOne "  " "" ""

showOne :: Text -> Text -> Text -> TreeFs -> [Text]
showOne leader tie arm (File fp _) =  [leader <> arm <> tie <> toText fp]
showOne leader tie arm (Dir fp (sortWith treeFp -> trees)) =
    nodeRep : showChildren trees (leader <> extension)
  where
    nodeRep :: Text
    nodeRep   = leader <> arm <> tie <> toText fp

    extension :: Text
    extension = case arm of ""  -> ""; "└" -> "    "; _   -> "│   "

showChildren :: [TreeFs] -> Text -> [Text]
showChildren children leader =
    let arms = replicate (length children - 1) "│" <> ["└"]
    in  concat (zipWith (showOne leader "── ") arms children)

-- For sorting in alphabetic order.
treeFp :: TreeFs -> FilePath
treeFp (Dir fp _)  = fp
treeFp (File fp _) = fp
