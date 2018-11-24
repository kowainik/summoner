{-# LANGUAGE ViewPatterns #-}

module Summoner.Tree
       ( TreeFs (..)
       , traverseTree
       , showBoldTree
       , showTree
       ) where

import System.Directory (createDirectoryIfMissing, withCurrentDirectory)

import Summoner.Ansi (boldCode, resetCode)

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
traverseTree (File name content) = writeFileText name content

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
                then boldCode <> str <> resetCode
                else str

        extension :: Text
        extension = case arm of ""  -> ""; "└" -> "    "; _   -> "│   "

    showChildren :: [TreeFs] -> Text -> [Text]
    showChildren children leader =
        let arms = replicate (length children - 1) "├" <> ["└"]
        in  concat (zipWith (showOne leader "── ") arms children)

-- For sorting in alphabetic order.
treeFp :: TreeFs -> FilePath
treeFp (Dir fp _)  = fp
treeFp (File fp _) = fp
