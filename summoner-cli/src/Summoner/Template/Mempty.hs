{- |
Copyright: (c) 2017-2019 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

'Monoid' utilities for templates.
-}

module Summoner.Template.Mempty
       ( memptyIfFalse
       ) where


{- | Returns the given value in case of the given 'Bool' is 'True'.
Otherwise, it returns 'mempty'.
-}
memptyIfFalse :: Monoid m => Bool -> m -> m
memptyIfFalse p val = if p then val else mempty
