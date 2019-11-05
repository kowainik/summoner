module Summoner.Template.Mempty
       ( memptyIfFalse
       ) where


{- | Returns the given value in case of the given 'Bool' is 'True'.
Otherwise, it returns 'mempty'.
-}
memptyIfFalse :: Monoid m => Bool -> m -> m
memptyIfFalse p val = if p then val else mempty
