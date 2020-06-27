{-# LANGUAGE ViewPatterns #-}

{- |
Module                  : Summoner.Tree
Copyright               : (c) 2017-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Data type for representing filesystem structure via tree.
-}

module Summoner.Tree
       ( TreeFs (..)
       , traverseTree
       , pathToTree
       , insertTree
       , showBoldTree
       , showTree
       ) where

import Colourista (bold, reset)
import System.Directory (createDirectoryIfMissing, withCurrentDirectory)
import System.FilePath (splitDirectories)


-- | Describes simple structure of filesystem tree.
data TreeFs
      -- | Name of directory (relative) and its containing entries
    = Dir FilePath [TreeFs]
      -- | File name (relative) and file content
    | File FilePath Text
    deriving stock (Generic, Show, Eq, Ord)

-- | Walks through directory tree and write file contents, creating all
-- intermediate directories.
traverseTree :: TreeFs -> IO ()
traverseTree (File name content) = writeFileText name content
traverseTree (Dir name children) = do
    createDirectoryIfMissing False name
    withCurrentDirectory name $ for_ children traverseTree

{- | This function converts a string file path to the tree structure.

For a path like this: @".github/workflow/ci.yml"@

This function produces the following tree:

@
.github/
└── workflow/
    └── ci.yml
@
-}
pathToTree :: FilePath -> Text -> TreeFs
pathToTree path content =
    let pathParts = splitDirectories path
    in case pathParts of
        []   -> Dir path []  -- shouldn't happen
        x:xs -> go x xs
  where
    go :: FilePath -> [FilePath] -> TreeFs
    go p []     = File p content
    go p (x:xs) = Dir p [go x xs]

{- | This functions inserts given 'TreeFs' node into the list of existing
'TreeFs' nodes. The behavior of this function is the following:

1. It merges duplicating directories.
2. It overrides existing 'File' with the given 'TreeFs' in case of duplicates.
-}
insertTree :: TreeFs -> [TreeFs] -> [TreeFs]
insertTree node [] = [node]
insertTree node (x:xs) = case (node, x) of
    (Dir _ _, File _ _) -> x : insertTree node xs
    (File _ _, Dir _ _) -> x : insertTree node xs
    (File nodePath _, File curPath _)
        | nodePath == curPath -> node : xs
        | otherwise -> x : insertTree node xs
    (Dir nodePath nodeChildren, Dir curPath curChildren)
        | nodePath == curPath ->
            Dir nodePath (foldr insertTree curChildren nodeChildren) : xs
        | otherwise -> x : insertTree node xs

-- | Pretty shows the directory tree content.
showBoldTree :: TreeFs -> Text
showBoldTree = showTree True

-- | Pretty shows tree with options.
showTree
    :: Bool    -- ^ Print directories bold.
    -> TreeFs  -- ^ Given tree.
    -> Text    -- ^ Pretty output.
showTree isBold = unlines . showOne "  " "" ""
  where
    showOne :: Text -> Text -> Text -> TreeFs -> [Text]
    showOne leader tie arm (File fp _) =  [leader <> arm <> tie <> toText fp]
    showOne leader tie arm (Dir fp (sortWith treeFp -> trees)) =
        nodeRep : showChildren trees (leader <> extension)
      where
        nodeRep :: Text
        nodeRep = leader <> arm <> tie <> boldDir (fp <> "/")
          where
            boldDir :: FilePath -> Text
            boldDir str = toText $
                if isBold
                then bold <> str <> reset
                else str

        extension :: Text
        extension = case arm of ""  -> ""; "└" -> "    "; _   -> "│   "

    showChildren :: [TreeFs] -> Text -> [Text]
    showChildren children leader =
        let arms = replicate (length children - 1) "├" <> ["└"]
        in  concat (zipWith (showOne leader "── ") arms children)

-- | Extract 'TreeFs' path. Used for sorting in alphabetic order.
treeFp :: TreeFs -> FilePath
treeFp (Dir fp _)  = fp
treeFp (File fp _) = fp
